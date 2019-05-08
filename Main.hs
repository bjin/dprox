{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Concurrent        (forkIO)
import           Control.Exception         (SomeException, handle, throwIO)
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
import           System.Posix.User         (UserEntry (..),
                                            getUserEntryForName, setUserID)

import           Config
import           DomainRoute

type Resolver = DNS.Domain -> DNS.TYPE -> IO (Either DNS.DNSError [DNS.RData])

processQuery :: Resolver -> DNS.Question -> IO [DNS.ResourceRecord]
processQuery resolver (DNS.Question qd qt) = handle handler $ do
    res <- resolver qd qt
    case res of
        Left e  -> return []
        Right r -> return (map wrapper r)
  where
    handler :: SomeException -> IO [DNS.ResourceRecord]
    handler _ = return []

    wrapper :: DNS.RData -> DNS.ResourceRecord
    wrapper rdata = DNS.ResourceRecord qd (getType rdata) DNS.classIN 233 rdata

    getType :: DNS.RData -> DNS.TYPE
    getType (DNS.RD_A _)     = DNS.A
    getType (DNS.RD_NS _)    = DNS.NS
    getType (DNS.RD_CNAME _) = DNS.CNAME
    getType (DNS.RD_MX _ _)  = DNS.MX
    getType (DNS.RD_TXT _)   = DNS.TXT
    getType (DNS.RD_AAAA _)  = DNS.AAAA
    getType _                = qt

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

handleServer :: DomainRoute (IP, PortNumber) -> M.Map (Maybe (IP, PortNumber)) Resolver -> Resolver
handleServer route resolvers qd qt = resolver qd qt
  where
    resolver = resolvers ! getDomainRouteByPrefix route qd

handleAddress :: DomainRoute [IP] -> Resolver -> Resolver
handleAddress route resolver qd qt =
    if null userDefined then resolver qd qt else return (Right userDefined)
  where
    ips = fromMaybe [] $ getDomainRouteExact route qd
    ipv4 = [DNS.RD_A ipv4    | IPv4 ipv4 <- ips]
    ipv6 = [DNS.RD_AAAA ipv6 | IPv6 ipv6 <- ips]

    userDefined | qt == DNS.A    = ipv4
                | qt == DNS.AAAA = ipv6
                | otherwise      = []

handleBogusNX :: S.Set IP -> Resolver -> Resolver
handleBogusNX blacklist resolver qd qt =
    fmap (fmap (filter (not.isBlacklisted))) $ resolver qd qt
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
        fallbackServer = "8.8.8.8"

        server  = [(fromMaybe "" mDomain, (ip, fromMaybe defaultPort mPort)) | Server mDomain ip mPort <- conf]
        address = [(domain, [ip]) | Address domain ip <- conf]
        bogusnx = [ip | BogusNX ip <- conf]

        serverRoute = newDomainRoute (flip const) server
        serverAddressSet = S.fromList $ F.toList serverRoute

        addressRoute = newDomainRoute (++) address

        bogusnxSet = S.fromList bogusnx

        resolvConf = DNS.defaultResolvConf { DNS.resolvCache = Just DNS.defaultCacheConf }

        resolvConfs = (Nothing, resolvConf { DNS.resolvInfo = DNS.RCHostName fallbackServer }) :
                    [ (Just addr, resolvConf { DNS.resolvInfo = rc })
                    | addr@(host, port) <- S.toList serverAddressSet
                    , let rc = if port == defaultPort
                               then DNS.RCHostName (show host)
                               else DNS.RCHostPort (show host) port
                    ]

    sock <- bindPortUDP (fromIntegral $ fromMaybe defaultPort localPort) (fromMaybe "*6" listenAddress)
    resolvSeeds <- forM resolvConfs $ \(k, v) -> do
        rs <- DNS.makeResolvSeed v
        return (k, rs)

    case setUser of
        Nothing  -> return ()
        Just uid -> setuid uid

    let processWithResolver resolver = forever $ do
            (bs, addr) <- recvFrom sock (fromIntegral DNS.maxUdpSize)
            forkIO $ do
                resp <- processDNS resolver bs
                case resp of
                    Left _      -> return ()
                    Right resp' -> sendTo sock resp' addr >> return ()

        createResolvers ((k,v):xs) m = DNS.withResolver v $ \rs ->
            createResolvers xs (M.insert k (DNS.lookup rs) m)
        createResolvers [] m = let resolver = handleBogusNX bogusnxSet $
                                              handleAddress addressRoute $
                                              handleServer serverRoute m
                               in processWithResolver resolver
    createResolvers resolvSeeds M.empty
