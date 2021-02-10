-- SPDX-License-Identifier: BSD-3-Clause
--
-- Copyright (C) 2019 Bin Jin. All Rights Reserved.
{-# LANGUAGE OverloadedStrings #-}
module Config
( GlobalConfig(..)
, Config(..)
, getConfig
, IP(..)
, invalidIPAddress
, PortNumber
) where

import           Control.Exception                (SomeException, handle)
import           Control.Monad                    (when)
import           Data.Attoparsec.ByteString       ((<?>))
import qualified Data.Attoparsec.ByteString       as P
import qualified Data.Attoparsec.ByteString.Char8 as P8
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as BS8
import           Data.IP                          (IP (..))
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   isNothing)
import           Data.Streaming.Network           (HostPreference)
import           Data.String                      (fromString)
import           Data.Version                     (showVersion)
import qualified Network.DNS                      as DNS
import           Network.Socket                   (PortNumber)
import           Options.Applicative
import           Text.Read                        (readMaybe)

import           Paths_dprox                      (version)

data GlobalConfig = GlobalConfig
    { setUser       :: Maybe String
    , localPort     :: Maybe PortNumber
    , listenAddress :: Maybe HostPreference
    , cacheSize     :: Int
    , cacheTTL      :: DNS.TTL
    } deriving (Eq, Show)

data Config = Server (Maybe DNS.Domain) IP (Maybe PortNumber)
            | Address DNS.Domain IP
            | Hosts DNS.Domain IP
            | BogusNX IP
    deriving (Eq, Show)

getConfig :: IO (GlobalConfig, [Config])
getConfig = do
    (globalConfigs, configFiles, hostsFiles, confs) <- execParser opts
    confs1 <- concat <$> mapM readConfigFromFile configFiles
    confs2 <- concat <$> mapM readHostsFromFile hostsFiles
    return (globalConfigs, confs1 ++ confs2 ++ confs)
  where
    opts = info ((,,,) <$> globalOption <*> configFileOption
                       <*> hostsFilesOption <*> plainOption <**> ver <**> helper)
      ( fullDesc <> progDesc desc )

    desc = "a lightweight DNS proxy server, supports a small subset of dnsmasq options"
    ver = infoOption (showVersion version) (long "version" <> help "show version")

    readConfigFromFile file = handle handler (parseConfigFile <$> BS.readFile file)
    readHostsFromFile file = handle handler (parseHostsFile <$> BS.readFile file)

    handler :: SomeException -> IO [Config]
    handler _ = return []

parseConfigFile :: BS.ByteString -> [Config]
parseConfigFile bs = case P.parseOnly parseFile bs of
    Left msg -> error msg
    Right r  -> r
  where
    parseFile = catMaybes <$> P.many' (parseLine parseConfig)

    parseConfig =
        parsePair "server" serverValue <|>
        parsePair "local" serverValue <|>
        parsePair "address" addressValue <|>
        parsePair "bogus-nxdomain" bogusNXValue

    parsePair optionName optionValue = do
        _ <- P8.string optionName
        skipSpaceTab
        _ <- P8.char '='
        skipSpaceTab
        res <- optionValue
        skipLine
        return res

parseHostsFile :: BS.ByteString -> [Config]
parseHostsFile bs = case P.parseOnly parseFile bs of
    Left msg -> error msg
    Right r  -> r
  where
    parseFile = catMaybes <$> P.many' (parseLine parseHosts)

    parseHosts = do
        parsedIP <- ip
        skipSpaceTab
        parsedDomain <- domain
        skipLine
        return (Hosts parsedDomain parsedIP)

parseLine :: P.Parser a -> P.Parser (Maybe a)
parseLine parser = do
    eof <- P.atEnd
    when eof $ fail "eof reached"
    skipSpaceTab
    (Just <$> parser) <|> (skipLine >> return Nothing)

skipSpaceTab :: P.Parser ()
skipSpaceTab = P.skipWhile P8.isHorizontalSpace

skipLine :: P.Parser ()
skipLine = do
    P.skipWhile (not . P8.isEndOfLine)
    P.skipWhile P8.isEndOfLine

domain :: P.Parser DNS.Domain
domain = P8.takeWhile1 (P8.inClass "-.a-zA-Z0-9") <?> "domain name"

ip :: P.Parser IP
ip = (<?> "IP address") $ do
    ipstr <- BS8.unpack <$> P8.takeWhile1 (P8.inClass "0-9.:")
    case readMaybe ipstr of
        Nothing     -> fail ("invalid IP address: " ++ ipstr)
        Just ipaddr -> return ipaddr

invalidIPAddress :: IP
invalidIPAddress = "::"

port :: P.Parser PortNumber
port = read . BS8.unpack <$> P.takeWhile1 P8.isDigit_w8 <?> "Port Number"

globalOption :: Parser GlobalConfig
globalOption = GlobalConfig <$> userOption
                            <*> portOption
                            <*> listenOption
                            <*> cacheOption
                            <*> ttlOption
  where
    userOption = optional $ strOption
        ( long "user"
       <> short 'u'
       <> metavar "<username>"
       <> help "Specify the userid to which dprox will change after startup")

    portOption = optional $ option auto
        ( long "port"
       <> short 'p'
       <> metavar "<port>"
       <> help "Listen on this port instead of 53")

    listenOption = optional $ fromString <$> strOption
        ( long "listen-address"
       <> short 'a'
       <> metavar "<ipaddr>"
       <> help "Listen on the given IP address")

    cacheOption = option auto
        ( long "cache-size"
       <> metavar "<integer>"
       <> value 4096
       <> help "Size of the cache in entries (default value: 4096), setting this to zero disables cache")

    ttlOption = option auto
        ( long "cache-ttl"
       <> metavar "<seconds>"
       <> value 233
       <> help "Cache TTL in seconds (default value: 233)")

configFileOption :: Parser [FilePath]
configFileOption = many $ strOption
    ( long "conf-file"
   <> short 'C'
   <> metavar "<file>"
   <> help "Configure file to read")

hostsFilesOption :: Parser [FilePath]
hostsFilesOption = combine <$> noHostsOption <*> many newHostsOption
  where
    combine False newHosts = "/etc/hosts" : newHosts
    combine True newHosts  = newHosts

    newHostsOption = strOption
        ( long "addn-hosts"
       <> short 'H'
       <> metavar "<file>"
       <> help "Additional hosts file to read other than /etc/hosts")

    noHostsOption = switch
        ( long "no-hosts"
       <> short 'h'
       <> help "Don't read /etc/hosts")

plainOption :: Parser [Config]
plainOption = (++) <$> many server <*> ((++) <$> many address <*> many bogusnx)
  where
    server = option (attoparsecReader serverValue)
        ( long "server"
       <> long "local"
       <> short 'S'
       <> metavar "[/<domain>/]<ipaddr>[#<port>]"
       <> help serverMsg)

    serverMsg = "Specify remote DNS server to use. " ++
                "If multiple servers are specified, only the last one will be used. " ++
                "If no server is specified, 8.8.8.8 will be used. " ++
                "If <domain> is specified, queries matching this domain or its subdomains will use use specified remote DNS server. " ++
                "If <ipaddr> is empty, queries matching specified domains will be handled by local hosts file only. " ++
                "<port> can be used to specify alternative port for DNS server."

    address = option (attoparsecReader addressValue)
        ( long "address"
       <> short 'A'
       <> metavar "[/<domain>/]<ipaddr>"
       <> help "For DNS queries matching <domain> or its subdomains, replies <ipaddr> directly")

    bogusnx = option (attoparsecReader bogusNXValue)
        ( long "bogus-nxdomain"
       <> short 'B'
       <> metavar "ip"
       <> help "Transform replies which contain the IP address given into \"No such domain\" replies")

attoparsecReader :: P.Parser a -> ReadM a
attoparsecReader p = eitherReader (P.parseOnly (p <* P.endOfInput) . BS8.pack)

serverValue :: P.Parser Config
serverValue = do
    parsedDomain <- optional (P8.char '/' *> domain <* P8.char '/')
    parsedIP <- optional ip
    when (isNothing parsedDomain && isNothing parsedIP) $ fail "at least one of <domain> and <ip> must be specified"
    parsedPort <- optional (P8.char '#' *> port)
    return (Server parsedDomain (fromMaybe invalidIPAddress parsedIP) parsedPort)

addressValue :: P.Parser Config
addressValue = Address <$> (P8.char '/' *> domain <* P8.char '/') <*> ip

bogusNXValue :: P.Parser Config
bogusNXValue = BogusNX <$> ip
