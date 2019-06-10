-- SPDX-License-Identifier: BSD-3-Clause
--
-- Copyright (C) 2019 Bin Jin. All Rights Reserved.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Concurrent        (forkIO)
import           Control.Exception         (SomeException, handle)
import           Control.Monad             (forM, forever)
import           Data.ByteString           (ByteString)
import qualified Data.Foldable             as F
import           Data.Map                  ((!))
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe)
import qualified Data.Set                  as S
import           Data.Streaming.Network    (bindPortUDP)
import qualified Network.DNS               as DNS
import           Network.Socket.ByteString (recvFrom, sendTo)
import           System.Posix.User         (UserEntry (..), getUserEntryForName,
                                            setUserID)

import           Config
import           DomainRoute

type Resolver = DNS.Domain -> DNS.TYPE -> IO (Either DNS.DNSError [DNS.RData])

processQuery :: Resolver -> DNS.Question -> IO [DNS.ResourceRecord]
processQuery resolver (DNS.Question qd qt) = handle handler $ do
    res <- resolver qd qt
    case res of
        Left _  -> return []
        Right r -> return (map wrapper r)
  where
    handler :: SomeException -> IO [DNS.ResourceRecord]
    handler _ = return []

    wrapper :: DNS.RData -> DNS.ResourceRecord
    wrapper rdata = DNS.ResourceRecord qd (getType rdata) DNS.classIN 233 rdata

    getType :: DNS.RData -> DNS.TYPE
    getType DNS.RD_A{}     = DNS.A
    getType DNS.RD_NS{}    = DNS.NS
    getType DNS.RD_CNAME{} = DNS.CNAME
    getType DNS.RD_SOA{}   = DNS.SOA
    getType DNS.RD_NULL{}  = DNS.NULL
    getType DNS.RD_PTR{}   = DNS.PTR
    getType DNS.RD_MX{}    = DNS.MX
    getType DNS.RD_TXT{}   = DNS.TXT
    getType DNS.RD_AAAA{}  = DNS.AAAA
    getType _              = qt

processDNS :: Resolver -> ByteString -> IO (Either DNS.DNSError ByteString)
processDNS resolver bs
    | Left e <- parse = return (Left e)
    | Right q <- parse = do
        rrs <- mapM (processQuery resolver) (DNS.question q)
        let hd = DNS.header DNS.defaultResponse
            resp = DNS.defaultResponse {
                DNS.header = hd { DNS.identifier = DNS.identifier (DNS.header q) },
                DNS.question = DNS.question q,
                DNS.answer = concat rrs
            }
        return (Right (DNS.encode resp))
  where
    parse = do
        q <- DNS.decode bs
        if DNS.qOrR (DNS.flags (DNS.header q)) == DNS.QR_Query then return q else Left DNS.FormatError

handleServer :: DomainRoute Resolver -> Resolver
handleServer route qd = resolver qd
  where
    resolver = fromMaybe (error "handleServer: internal error") (getDomainRouteByPrefix route qd)

handleAddressAndHosts :: DomainRoute [IP] -> DomainRoute [IP] -> Resolver -> Resolver
handleAddressAndHosts address hosts resolver qd qt =
    if null ips then resolver qd qt else return (Right userDefined)
  where
    ips1 = fromMaybe [] $ getDomainRouteByPrefix address qd
    ips2 = fromMaybe [] $ getDomainRouteExact hosts qd
    ips | null ips2 = ips1
        | otherwise = ips2

    ipv4 = [DNS.RD_A    ipv4addr | IPv4 ipv4addr <- ips]
    ipv6 = [DNS.RD_AAAA ipv6addr | IPv6 ipv6addr <- ips]

    userDefined | qt == DNS.A    = ipv4
                | qt == DNS.AAAA = ipv6
                | otherwise      = []

handleBogusNX :: S.Set IP -> Resolver -> Resolver
handleBogusNX blacklist resolver qd qt =
    fmap (filter (not . isBlacklisted)) <$> resolver qd qt
  where
    isBlacklisted (DNS.RD_A ipv4)    = IPv4 ipv4 `S.member` blacklist
    isBlacklisted (DNS.RD_AAAA ipv6) = IPv6 ipv6 `S.member` blacklist
    isBlacklisted _                  = False

setuid :: String -> IO ()
setuid user = getUserEntryForName user >>= setUserID . userID

main :: IO ()
main = do
    (GlobalConfig{..}, conf) <- getConfig
    let defaultPort = 53
        fallbackServer = Server Nothing "8.8.8.8" Nothing

        server  = [ (fromMaybe "" mDomain, (ip, fromMaybe defaultPort mPort))
                  | Server mDomain ip mPort <- fallbackServer : conf
                  ]
        address = [(domain, [ip]) | Address domain ip <- conf]
        hosts = [(domain, [ip]) | Hosts domain ip <- conf]
        bogusnx = [ip | BogusNX ip <- conf]

        serverRoute = newDomainRoute (flip const) server
        serverAddressSet = S.fromList $ F.toList serverRoute

        addressRoute = newDomainRoute (++) address
        hostsRoute = newDomainRoute (++) hosts

        bogusnxSet = S.fromList bogusnx

        resolvConfs = [ (addr, rc)
                      | addr@(host, port) <- S.toList serverAddressSet
                      , let rsinfo = if port == defaultPort
                                then DNS.RCHostName (show host)
                                else DNS.RCHostPort (show host) port
                      , let rc = DNS.defaultResolvConf {
                            DNS.resolvCache = Just DNS.defaultCacheConf,
                            DNS.resolvInfo = rsinfo
                        }
                      ]

    sock <- bindPortUDP (fromIntegral $ fromMaybe defaultPort localPort) (fromMaybe "*6" listenAddress)
    resolvSeeds <- forM resolvConfs $ \(k, v) -> do
        rs <- DNS.makeResolvSeed v
        return (k, rs)

    F.mapM_ setuid setUser

    let processWithResolver resolver = forever $ do
            (bs, addr) <- recvFrom sock (fromIntegral DNS.maxUdpSize)
            forkIO $ do
                resp <- processDNS resolver bs
                F.forM_ resp $ \resp' -> sendTo sock resp' addr

        createResolvers ((k,v):xs) m = DNS.withResolver v $ \rs ->
            createResolvers xs (M.insert k (DNS.lookup rs) m)
        createResolvers [] m = let serverRoute' = fmap (m!) serverRoute
                                   resolver = handleBogusNX bogusnxSet $
                                              handleAddressAndHosts addressRoute hostsRoute $
                                              handleServer serverRoute'
                               in processWithResolver resolver
    createResolvers resolvSeeds M.empty
