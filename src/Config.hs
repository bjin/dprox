-- SPDX-License-Identifier: BSD-3-Clause
--
-- Copyright (C) 2019 Bin Jin. All Rights Reserved.
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , GlobalConfig(..)
  , IP(..)
  , IPMask
  , IPSetMatch(..)
  , PortNumber
  , getConfig
  , invalidIPAddress
  , parseConfigFile
  ) where

import Control.Exception                (IOException, try)
import Control.Monad                    (when)
import Data.Attoparsec.ByteString       ((<?>))
import Data.Attoparsec.ByteString       qualified as P
import Data.Attoparsec.ByteString.Char8 qualified as P8
import Data.ByteString                  qualified as BS
import Data.ByteString.Char8            qualified as BS8
import Data.IP                          (AddrRange, IP(..), IPv4)
import Data.Maybe                       (fromMaybe, isNothing)
import Data.Streaming.Network           (HostPreference)
import Data.String                      (fromString)
import Data.Version                     (showVersion)
import Data.Word                        (Word8)
import Network.DNS                      qualified as DNS
import Network.Socket                   (PortNumber)
import Options.Applicative
import System.Exit                      (die)
import Text.Read                        (readMaybe)

import Log
import Paths_dprox

data GlobalConfig = GlobalConfig
    { localPort     :: !(Maybe PortNumber)
    , listenAddress :: !(Maybe HostPreference)
    , cacheSize     :: !Int
    , cacheTTL      :: !DNS.TTL
    , loglevel      :: !LogLevel
    , ipsetMatch    :: !IPSetMatch
    , ipsetServer   :: !(Maybe (IP, Maybe PortNumber))
    , user          :: !(Maybe String)
    , group         :: !(Maybe String)
    }
  deriving (Eq, Show)

type IPMask = AddrRange IPv4

data Config = Server !(Maybe DNS.Domain) !IP !(Maybe PortNumber)
            | Address !DNS.Domain !IP
            | Hosts !DNS.Domain !IP
            | BogusNX !IP
            | IPSet !IPMask
  deriving (Eq, Show)

data IPSetMatch = NoneMatch
                | AllMatch
                | AnyMatch
                | NotAllMatch
  deriving (Eq, Show)

getConfig :: IO (GlobalConfig, [Config])
getConfig = do
    (confs, globalConfigs, configFiles, noHosts, hostsFiles, ipsetFiles) <- execParser opts
    confs1 <- concat <$> mapM (readParsedConfigFile "config file" parseConfigFile) configFiles
    confs2 <- readDefaultHostsFile noHosts
    confs3 <- concat <$> mapM (readParsedConfigFile "hosts file" parseHostsFile) hostsFiles
    confs4 <- concat <$> mapM (readParsedConfigFile "ipset file" parseIPSetFile) ipsetFiles
    return (globalConfigs, confs1 ++ confs2 ++ confs3 ++ confs4 ++ confs)
  where
    opts = info ((,,,,,) <$> plainOption <*> globalOption <*> configFileOption
                         <*> noHostsOption <*> hostsFilesOption <*> ipsetFileOption <**> ver <**> helper)
      ( fullDesc <> progDesc desc )

    desc = "a lightweight DNS proxy server, supports a small subset of dnsmasq options"
    ver = infoOption (showVersion version) (long "version" <> help "show version")

    readDefaultHostsFile True = return []
    readDefaultHostsFile False = readOptionalParsedConfigFile "hosts file" parseHostsFile "/etc/hosts"

readParsedConfigFile
    :: String
    -> (BS.ByteString -> Either String [Config])
    -> FilePath
    -> IO [Config]
readParsedConfigFile sourceName parser file = do
    readResult <- try (BS.readFile file) :: IO (Either IOException BS.ByteString)
    case readResult of
        Left exception -> die (readConfigError sourceName file exception)
        Right content  -> parsedConfig sourceName file parser content

readOptionalParsedConfigFile
    :: String
    -> (BS.ByteString -> Either String [Config])
    -> FilePath
    -> IO [Config]
readOptionalParsedConfigFile sourceName parser file = do
    readResult <- try (BS.readFile file) :: IO (Either IOException BS.ByteString)
    case readResult of
        Left _        -> return []
        Right content -> parsedConfig sourceName file parser content

parsedConfig
    :: String
    -> FilePath
    -> (BS.ByteString -> Either String [Config])
    -> BS.ByteString
    -> IO [Config]
parsedConfig sourceName file parser content =
    case parser content of
        Left parseError -> die ("failed to parse " ++ sourceName ++ " " ++ file ++ ": " ++ parseError)
        Right confs     -> return confs

readConfigError :: String -> FilePath -> IOException -> String
readConfigError sourceName file exception =
    "failed to read " ++ sourceName ++ " " ++ file ++ ": " ++ show exception

parseConfigFile :: BS.ByteString -> Either String [Config]
parseConfigFile = P.parseOnly (parseLines parseConfigLine)
  where
    parseConfigLine line
        | isIgnoredLine line = Right Nothing
        | isConfigOption "server" line = parseConfigPairLine "server" serverValue line
        | isConfigOption "local" line = parseConfigPairLine "local" serverValue line
        | isConfigOption "address" line = parseConfigPairLine "address" addressValue line
        | isConfigOption "bogus-nxdomain" line = parseConfigPairLine "bogus-nxdomain" bogusNXValue line
        | otherwise = Left ("unsupported config directive: " ++ BS8.unpack (trimLine line))

parseHostsFile :: BS.ByteString -> Either String [Config]
parseHostsFile = P.parseOnly (parseLines parseHostsLine)
  where
    parseHostsLine line
        | isIgnoredLine line = Right Nothing
        | startsWithIPToken line = Just <$> P.parseOnly parseHosts line
        | otherwise = Right Nothing

    parseHosts = do
        skipSpaceTab
        parsedIP <- ip
        skipSpaceTab1
        parsedDomain <- domain
        return (Hosts parsedDomain parsedIP)

parseIPSetFile :: BS.ByteString -> Either String [Config]
parseIPSetFile = P.parseOnly (parseLines parseIPSetLine)
  where
    parseIPSetLine line
        | isIgnoredLine line = Right Nothing
        | otherwise = Just <$> P.parseOnly parseIPSet line

    parseIPSet = IPSet <$> (skipSpaceTab *> ip4mask <* skipLineRest)

parseLines :: (BS.ByteString -> Either String (Maybe a)) -> P.Parser [a]
parseLines parseLineValue = go (1 :: Int) []
  where
    go lineNumber acc = do
        eof <- P.atEnd
        if eof
        then return (reverse acc)
        else do
            line <- takeLine
            case parseLineValue line of
                Left msg                 -> fail ("line " ++ show lineNumber ++ ": " ++ msg)
                Right Nothing            -> go (lineNumber + 1) acc
                Right (Just parsedValue) -> go (lineNumber + 1) (parsedValue : acc)

takeLine :: P.Parser BS.ByteString
takeLine = do
    line <- P.takeTill P8.isEndOfLine
    skipEndOfLine
    return line

skipEndOfLine :: P.Parser ()
skipEndOfLine = do
    eof <- P.atEnd
    if eof
    then return ()
    else do
        c <- P.satisfy P8.isEndOfLine
        when (c == char8 '\r') $ do
            _ <- optional (P.satisfy (== char8 '\n'))
            return ()

parseConfigPairLine
    :: BS.ByteString
    -> P.Parser Config
    -> BS.ByteString
    -> Either String (Maybe Config)
parseConfigPairLine optionName optionValue line =
    Just <$> P.parseOnly (parseConfigPair optionName optionValue) line

parseConfigPair :: BS.ByteString -> P.Parser Config -> P.Parser Config
parseConfigPair optionName optionValue = do
    skipSpaceTab
    _ <- P.string optionName
    skipSpaceTab
    _ <- P8.char '='
    skipSpaceTab
    optionValue <* skipLineRest

isIgnoredLine :: BS.ByteString -> Bool
isIgnoredLine line =
    case BS.uncons (trimLine line) of
        Nothing     -> True
        Just (c, _) -> c == char8 '#'

isConfigOption :: BS.ByteString -> BS.ByteString -> Bool
isConfigOption optionName line =
    case BS.stripPrefix optionName (trimLine line) of
        Nothing        -> False
        Just remainder -> isOptionBoundary remainder

isOptionBoundary :: BS.ByteString -> Bool
isOptionBoundary remainder =
    case BS.uncons remainder of
        Nothing     -> True
        Just (c, _) -> c == char8 '=' || P8.isHorizontalSpace c

startsWithIPToken :: BS.ByteString -> Bool
startsWithIPToken line =
    let token = BS.takeWhile (not . P8.isHorizontalSpace) (trimLine line)
    in not (BS.null token) && BS.all isIPTokenByte token && BS.any P8.isDigit_w8 token

isIPTokenByte :: Word8 -> Bool
isIPTokenByte c = P8.isDigit_w8 c || c == char8 '.' || c == char8 ':'

trimLine :: BS.ByteString -> BS.ByteString
trimLine = BS.dropWhile P8.isHorizontalSpace

char8 :: Char -> Word8
char8 = fromIntegral . fromEnum

skipSpaceTab1 :: P.Parser ()
skipSpaceTab1 = P.satisfy P8.isHorizontalSpace *> skipSpaceTab
skipSpaceTab :: P.Parser ()
skipSpaceTab = P.skipWhile P8.isHorizontalSpace

skipLineRest :: P.Parser ()
skipLineRest = do
    eof <- P.atEnd
    if eof
    then return ()
    else do
        skipSpaceTab1
        _ <- optional (P8.char '#' *> P.takeByteString)
        P.endOfInput

domain :: P.Parser DNS.Domain
domain = P8.takeWhile1 (P8.inClass "-.a-zA-Z0-9") <?> "domain name"

ip :: P.Parser IP
ip = (<?> "IP address") $ do
    ipstr <- BS8.unpack <$> P8.takeWhile1 (P8.inClass "0-9.:")
    case readMaybe ipstr of
        Nothing     -> fail ("invalid IP address: " ++ ipstr)
        Just ipaddr -> return ipaddr

ip4mask :: P.Parser IPMask
ip4mask = (<?> "IPv4 mask") $ do
    ipstr <- BS8.unpack <$> P8.takeWhile1 (P8.inClass "0-9./")
    case readMaybe ipstr of
        Nothing     -> fail ("invalid IP mask: " ++ ipstr)
        Just ipmask -> return ipmask

port :: P.Parser PortNumber
port = (<?> "Port Number") $ do
    portValue <- P8.decimal
    if portValue <= maxPortNumber
    then return (fromIntegral (portValue :: Integer))
    else fail "port number must be between 0 and 65535"
  where
    maxPortNumber = 65535 :: Integer

portSuffix :: P.Parser (Maybe PortNumber)
portSuffix = do
    hasPort <- P.option False (True <$ P8.char '#')
    if hasPort
    then Just <$> port
    else return Nothing

ipport :: P.Parser (IP, Maybe PortNumber)
ipport = (,) <$> ip <*> portSuffix

invalidIPAddress :: IP
invalidIPAddress = "::"

globalOption :: Parser GlobalConfig
globalOption = GlobalConfig <$> portOption
                            <*> listenOption
                            <*> cacheOption
                            <*> ttlOption
                            <*> loglevelOption
                            <*> ipsetMatchOption
                            <*> ipsetServerOption
#ifdef OS_UNIX
                            <*> userOption
                            <*> groupOption
#else
                            <*> pure Nothing
                            <*> pure Nothing
#endif
  where
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

    loglevelOption = option (maybeReader logLevelReader)
        ( long "log-level"
       <> metavar "<trace|debug|info|warn|error|none>"
       <> value INFO
       <> help "specify the logging level (default: info)")

    ipsetMatchOption = option (maybeReader ipsetMatchReader)
        ( long "ipset-match"
       <> metavar "<none|all|any|notall>"
       <> value AnyMatch
       <> help ("matching policy for --ipset (default value: any). Note that the matching procedure will " ++
                "be performed only on DNS response with at least one IPv4 address."))

    ipsetServerOption = optional $ option (attoparsecReader ipport)
        ( long "ipset-server"
       <> metavar "<ipaddr>[#port]"
       <> help "DNS server to use if ipset matches DNS response")

    ipsetMatchReader "none"   = Just NoneMatch
    ipsetMatchReader "all"    = Just AllMatch
    ipsetMatchReader "any"    = Just AnyMatch
    ipsetMatchReader "notall" = Just NotAllMatch
    ipsetMatchReader _        = Nothing

#ifdef OS_UNIX
    userOption = optional $ strOption
        ( long "user"
       <> short 'u'
       <> metavar "nobody"
       <> help "drop root priviledge and setuid to the specified user (like nobody)")

    groupOption = optional $ strOption
        ( long "group"
       <> short 'g'
       <> metavar "nogroup"
       <> help "drop root priviledge and setgid to the specified group")
#endif

configFileOption :: Parser [FilePath]
configFileOption = many $ strOption
    ( long "conf-file"
   <> short 'C'
   <> metavar "<file>"
   <> help "Configure file to read")

noHostsOption :: Parser Bool
noHostsOption = switch
    ( long "no-hosts"
   <> short 'h'
   <> help "Don't read /etc/hosts")

hostsFilesOption :: Parser [FilePath]
hostsFilesOption = many $ strOption
    ( long "addn-hosts"
   <> short 'H'
   <> metavar "<file>"
   <> help "Additional hosts file to read other than /etc/hosts")

ipsetFileOption :: Parser [FilePath]
ipsetFileOption = many $ strOption
    ( long "ipset-file"
   <> metavar "<file>"
   <> help "Read ipset from files")

plainOption :: Parser [Config]
plainOption = (++) <$> many server <*> ((++) <$> many address <*> ((++) <$> many bogusnx <*> many ipset))
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
       <> metavar "<ipaddr>"
       <> help "Transform replies which contain the IP address given into \"No such domain\" replies")

    ipset = option (attoparsecReader (IPSet <$> ip4mask))
        ( long "ipset"
       <> metavar "<ipmask>"
       <> help ipsetMsg)

    ipsetMsg = "Add an IPv4 address to ipset. If DNS responses matches ipset, DNS responses from " ++
               "\"--ipset-server\" will be returned instead. Exact matching policy can be controlled " ++
               "by \"--ipset-match\". Note that this option is different from the option with the same name from dnsmasq."

attoparsecReader :: P.Parser a -> ReadM a
attoparsecReader p = eitherReader (P.parseOnly (p <* P.endOfInput) . BS8.pack)

serverValue :: P.Parser Config
serverValue = do
    parsedDomain <- optional (P8.char '/' *> domain <* P8.char '/')
    parsedIP <- optional ip
    when (isNothing parsedDomain && isNothing parsedIP) $ fail "at least one of <domain> and <ip> must be specified"
    parsedPort <- portSuffix
    return (Server parsedDomain (fromMaybe invalidIPAddress parsedIP) parsedPort)

addressValue :: P.Parser Config
addressValue = Address <$> (P8.char '/' *> domain <* P8.char '/') <*> ip

bogusNXValue :: P.Parser Config
bogusNXValue = BogusNX <$> ip
