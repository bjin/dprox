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
            | BogusNX IP
    deriving (Eq, Show)

getConfig :: IO (GlobalConfig, [Config])
getConfig = do
    (globalConfigs, configFiles, confs) <- execParser opts
    confs' <- concat <$> mapM readConfigFromFile configFiles
    return (globalConfigs, confs' ++ confs)
  where
    opts = info ((,,) <$> globalOption <*> configFileOption <*> plainOption <**> helper)
      ( fullDesc <> progDesc "a simple DNS proxy server")

    readConfigFromFile file = handle handler (parseConfigFile <$> BS.readFile file)

    handler :: SomeException -> IO [Config]
    handler _ = return []

parseConfigFile :: BS.ByteString -> [Config]
parseConfigFile bs = case P.parseOnly parseFile bs of
    Left msg -> error msg
    Right r  -> r
  where
    skipSpaceTab = P.skipWhile P8.isHorizontalSpace

    parseFile = catMaybes <$> P.many' parseLine

    parseLine = do
        eof <- P.atEnd
        when eof $ fail "eof reached"
        skipSpaceTab
        (Just <$> parseConfig) <|> skipLine

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
        _ <- skipLine
        return res

    skipLine = do
        P.skipWhile (not . P8.isEndOfLine)
        P.skipWhile P8.isEndOfLine
        return Nothing

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
