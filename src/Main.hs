-- SPDX-License-Identifier: BSD-3-Clause
--
-- Copyright (C) 2019 Bin Jin. All Rights Reserved.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import Control.Concurrent        (forkIO, threadDelay)
import Control.Exception         (SomeException, handle)
import Control.Monad             (forM, forever, join)
import Data.ByteString           (ByteString)
import Data.Foldable             qualified as F
import Data.Hashable             (Hashable (..))
import Data.IP                   qualified as IP
import Data.IP.RouteTable        qualified as IP
import Data.Map                  qualified as M
import Data.Maybe                (fromMaybe, isJust, maybeToList)
import Data.Set                  qualified as S
import Data.Streaming.Network    (bindPortUDP)
import Network.DNS               qualified as DNS
import Network.Socket.ByteString (recvFrom, sendTo)
import System.Posix.User
    (UserEntry (..), getUserEntryForName, setUserID)

import Config
import DomainRoute
import LRU

instance Hashable DNS.TYPE where
    hashWithSalt s = hashWithSalt s . DNS.fromTYPE

type Resolver = DNS.Domain -> DNS.TYPE -> IO (Either DNS.DNSError [DNS.RData])
type CachedResolver = DNS.Domain -> DNS.TYPE -> IO (Either DNS.DNSError (DNS.TTL, [DNS.RData]))

processQuery :: CachedResolver -> DNS.Question -> IO [DNS.ResourceRecord]
processQuery resolver (DNS.Question qd qt) = handle handler $ do
    res <- resolver qd qt
    case res of
        Left _         -> return []
        Right (ttl, r) -> return (map (wrapper ttl) r)
  where
    handler :: SomeException -> IO [DNS.ResourceRecord]
    handler _ = return []

    wrapper :: DNS.TTL -> DNS.RData -> DNS.ResourceRecord
    wrapper ttl rdata = DNS.ResourceRecord qd (getType rdata) DNS.classIN ttl rdata

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

processDNS :: CachedResolver -> ByteString -> IO (Either DNS.DNSError ByteString)
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

handleServer :: DomainRoute (Maybe Resolver) -> Resolver
handleServer route qd qt = case resolver of
    Nothing        -> return (Left DNS.NameError)
    Just resolver' -> resolver' qd qt
  where
    resolver = join (getDomainRouteByPrefix route qd)

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

handleIPSet :: [IPMask] -> IPSetMatch -> Maybe Resolver -> LRUCache DNS.Domain () -> CachedResolver -> CachedResolver
handleIPSet [] _ _ _ = id
handleIPSet _ _ Nothing _ = id
handleIPSet ipset match (Just ipsetResolver) cache = handleWithResolver
  where
    iproute = IP.fromList $ zip ipset $ repeat ()
    inIPSet ip = isJust $ IP.lookup (IP.makeAddrRange ip 32) iproute

    check NoneMatch   = not . any inIPSet
    check AllMatch    = all inIPSet
    check AnyMatch    = any inIPSet
    check AnyNotMatch = any (not.inIPSet)

    handleWithResolver resolver qd qt@DNS.A = do
        cachedInIPSet <- isJust <$> lookupCache qd cache
        if cachedInIPSet
          then fmap (1,) <$> ipsetResolver qd qt
          else do
            res <- resolver qd qt
            case res of
                Left _        -> return res
                Right (_, rs) -> do
                    let ipv4s = [ipv4 | DNS.RD_A ipv4 <- rs]
                    if not (null ipv4s) && check match ipv4s
                      then do
                        updateCache qd () cache
                        fmap (1,) <$> ipsetResolver qd qt
                      else return res
    handleWithResolver resolver qd qt = resolver qd qt

makeResolverCache :: Int -> DNS.TTL -> IO (Resolver -> CachedResolver)
makeResolverCache sz ttl | sz <= 0 = return $ \r qd qt -> fmap (ttl,) <$> r qd qt
makeResolverCache sz ttl = do
    cache <- newCache sz ttl
    _ <- forkIO $ forever $ do
        threadDelay (fromInteger (fromIntegral ttl * 1000000 `div` 3))
        purgeCache cache
    let process resolver qd qt = do
            let k = (qd, qt)
            res <- lookupCache k cache
            case res of
                Just (ttl', v) -> return (Right (max ttl' 3, v))
                Nothing        -> do
                    resolved <- resolver qd qt
                    case resolved of
                        Left  e -> return (Left e)
                        Right v -> do
                            updateCache k v cache
                            return (Right (ttl, v))
    return process

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
        ipset = [ipmask | IPSet ipmask <- conf]

        serverRoute = newDomainRoute (const id) server
        serverAddressSet = S.fromList $ F.toList serverRoute

        addressRoute = newDomainRoute (++) address
        hostsRoute = newDomainRoute (++) hosts

        bogusnxSet = S.fromList bogusnx

        ipsetServerPort = fmap (fromMaybe defaultPort) <$> ipsetServer

        resolvConfs = [ (addr, rc)
                      | addr@(host, port) <- S.toList serverAddressSet ++ maybeToList ipsetServerPort
                      , host /= invalidIPAddress
                      , let rsinfo = if port == defaultPort
                                then DNS.RCHostName (show host)
                                else DNS.RCHostPort (show host) port
                      , let rc = DNS.defaultResolvConf {
                            DNS.resolvCache = Nothing,
                            DNS.resolvInfo = rsinfo
                        }
                      ]

    sock <- bindPortUDP (fromIntegral $ fromMaybe defaultPort localPort) (fromMaybe "*6" listenAddress)
    resolvSeeds <- forM resolvConfs $ \(k, v) -> do
        rs <- DNS.makeResolvSeed v
        return (k, rs)

    F.mapM_ setuid setUser

    resolverCache <- makeResolverCache cacheSize cacheTTL
    ipsetCache <- newCache 4096 maxBound

    let processWithResolver resolver = forever $ do
            (bs, addr) <- recvFrom sock (fromIntegral DNS.maxUdpSize)
            forkIO $ do
                resp <- processDNS resolver bs
                F.forM_ resp $ \resp' -> sendTo sock resp' addr

        createResolvers ((k,v):xs) m = DNS.withResolver v $ \rs ->
            createResolvers xs (M.insert k (DNS.lookup rs) m)
        createResolvers [] m = let serverRoute' = fmap (`M.lookup`m) serverRoute
                                   ipsetResolver = join $ fmap (`M.lookup`m) ipsetServerPort
                                   resolver = handleIPSet ipset ipsetMatch ipsetResolver ipsetCache $
                                              resolverCache $
                                              handleAddressAndHosts addressRoute hostsRoute $
                                              handleBogusNX bogusnxSet $
                                              handleServer serverRoute'
                               in processWithResolver resolver
    createResolvers resolvSeeds M.empty
