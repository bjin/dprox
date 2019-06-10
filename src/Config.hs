-- SPDX-License-Identifier: BSD-3-Clause
--
-- Copyright (C) 2019 Bin Jin. All Rights Reserved.
{-# LANGUAGE OverloadedStrings #-}
module Config
( GlobalConfig(..)
, Config(..)
, getConfig
, IP(..)
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
import           Data.Maybe                       (catMaybes)
import           Data.Streaming.Network           (HostPreference)
import           Data.String                      (fromString)
import qualified Network.DNS                      as DNS
import           Network.Socket                   (PortNumber)
import           Options.Applicative
import           Text.Read                        (readMaybe)

data GlobalConfig = GlobalConfig
    { setUser       :: Maybe String
    , localPort     :: Maybe PortNumber
    , listenAddress :: Maybe HostPreference
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
                       <*> hostsFilesOption <*> plainOption <**> helper)
      ( fullDesc <> progDesc "a simple DNS proxy server")

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

port :: P.Parser PortNumber
port = read . BS8.unpack <$> P.takeWhile1 P8.isDigit_w8 <?> "Port Number"

globalOption :: Parser GlobalConfig
globalOption = GlobalConfig <$> userOption
                            <*> portOption
                            <*> listenOption
  where
    userOption = optional $ strOption
        ( long "user"
       <> short 'u'
       <> metavar "uid"
       <> help "set user id")

    portOption = optional $ option auto
        ( long "port"
       <> short 'p'
       <> metavar "port"
       <> help "listen on this port instead of 53")

    listenOption = optional $ fromString <$> strOption
        ( long "listen-address"
       <> short 'a'
       <> metavar "ipaddr"
       <> help "Listen on the given IP address")

configFileOption :: Parser [FilePath]
configFileOption = many $ strOption
    ( long "conf-file"
   <> short 'C'
   <> metavar "path/to/dprox.conf"
   <> help "configure file to read")

hostsFilesOption :: Parser [FilePath]
hostsFilesOption = combine <$> noHostsOption <*> many newHostsOption
  where
    combine False newHosts = "/etc/hosts" : newHosts
    combine True newHosts = newHosts

    newHostsOption = strOption
        ( long "addn-hosts"
       <> short 'H'
       <> metavar "path/to/hosts"
       <> help "additional hosts file to read (other than /etc/hosts)")

    noHostsOption = switch
        ( long "no-hosts"
       <> short 'h'
       <> help "Don't read /etc/hosts")

plainOption :: Parser [Config]
plainOption = (++) <$> many server <*> ((++) <$> many address <*> many bogusnx)
  where
    server = option (attoparsecReader serverValue)
        ( long "server"
       <> short 'S'
       <> metavar "[/domain/]ip[#port]"
       <> help "remote dns server ip")

    address = option (attoparsecReader addressValue)
        ( long "address"
       <> short 'A'
       <> metavar "/domain/ip"
       <> help "specifiy ip for target domain")

    bogusnx = option (attoparsecReader bogusNXValue)
        ( long "bogus-nxdomain"
       <> short 'B'
       <> metavar "ip"
       <> help "specify ip for no such domain blacklist")

attoparsecReader :: P.Parser a -> ReadM a
attoparsecReader p = eitherReader (P.parseOnly (p <* P.endOfInput) . BS8.pack)

serverValue :: P.Parser Config
serverValue = do
    parsedDomain <- P.option Nothing (Just <$> (P8.char '/' *> domain <* P8.char '/'))
    parsedIP <- ip
    parsedPort <- P.option Nothing (Just <$> (P8.char '#' *> port))
    return (Server parsedDomain parsedIP parsedPort)

addressValue :: P.Parser Config
addressValue = Address <$> (P8.char '/' *> domain <* P8.char '/') <*> ip

bogusNXValue :: P.Parser Config
bogusNXValue = BogusNX <$> ip
