module PortfolioSink where

import Control.Monad (liftM)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TI
import Database.CouchDB
import Network.FastCGI
import System.IO
import Text.Parsec
import Text.Parsec.Text
import Common
import Nordnet

-- |Does the actual parsing and pushing of results. Fails when
-- something goes wrong.
portfolioSink :: CouchConn -> CGI CGIResult
portfolioSink conn = do
  -- Getting the stuff from a request. Making Text strict a bit ugly
  -- way because CGI library is quite poor.
  id <- requestHeader "PortfolioID" `orFail` "Portfolio ID is not defined"
  format <- requestHeader "PortfolioFormat" `orFail` "Portfolio format is not defined"
  raw <- liftM (decodeUtf8 . BS.concat . B.toChunks) getBodyFPS
  
  -- Get temporary file and write the portfolio on disk for debugging.
  f <- liftIO $ do 
    (f,h) <- openBinaryTempFile "portfolio_log" ".csv"
    TI.hPutStr h raw
    hClose h
    return f

  -- Dummy access log.
  liftIO $ appendFile "portfolio.log" $ concat ["id ",id," format ",format,
                                                " file ",f,"\n"]

  -- Parser chooser
  parser <- case lookup format parsers of 
    Just x -> return x
    Nothing -> fail $ "Format of " ++ format ++ " is not supported"

  let info = PortfolioInfo { pId = id, tmpFile = f }
  liftIO $ parseAndSend conn (parser info) raw

  output "ok\r\n"

-- |Lookup table for parsers.
parsers = [("nordnet",nordnet)]

-- |Parses and sends the portfolio
parseAndSend :: CouchConn -> Parser [Record] -> Text -> IO ()
parseAndSend conn p raw = parsePortfolio p raw >>= sendPortfolio conn

-- |Just parses and fails monadically
parsePortfolio :: (Monad m) => Parser [Record] -> Text -> m [Record]
parsePortfolio p raw =
  case parse p "portfolio" raw of
    Left e -> fail $ "Parsing of portfolio failed in " ++ show (errorPos e)
    Right a -> return a

sendPortfolio :: CouchConn -> [Record] -> IO ()
sendPortfolio conn pf = runCouchDBWith conn $ do
  mapM_ maybeUpdate pf
  where
    maybeUpdate (doc,json) = do
      ret <- newNamedDoc portfolioDb doc json
      -- This may fail but it's not very probable. Conflight requires
      -- two simultanous portofolio updates when it's ok anyway to 
      -- ignore the other anyway.
      case ret of
        Left _ -> getAndUpdateDoc portfolioDb doc (return . (const json))
        Right _ -> return Nothing -- Not interested
