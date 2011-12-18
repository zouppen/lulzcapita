module Main where

import Control.Monad (liftM,unless)
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.IO as TI
import Database.CouchDB
import Network.FastCGI
import Network.URI
import System.Environment
import System.IO
import Common
import Nordnet

main = do
  -- TODO Migrate to cmdargs when we more parameters than one. Now we
  -- have very brutal commandline parser which dies on errors.
  [dbStr] <- getArgs
  let Just dbUri = parseURI dbStr
  
  -- Create connection
  conn <- createCouchConnFromURI dbUri
  
  -- Runs FastCGI and outputs exceptions as internal server
  -- errors. TODO more sophisticated messages?
  runFastCGI $ catchCGI (portfolioSink conn) outputException

-- |Does the actual parsing and pushing of results. Fails when
-- something goes wrong.
portfolioSink :: CouchConn -> CGI CGIResult
portfolioSink conn = do
  -- Getting the stuff from a request
  id <- requestHeader "PortfolioID" `orFail` "Portfolio ID is not defined"
  format <- requestHeader "PortfolioFormat" `orFail` "Portfolio format is not defined"
  raw <- liftM decodeUtf8 getBodyFPS
  
  -- Get temporary file and write the portfolio on disk for debugging.
  f <- liftIO $ do 
    (f,h) <- openBinaryTempFile "portfolio_log" ".csv"
    TI.hPutStr h raw
    hClose h
    return f

  -- Dummy access log.
  liftIO $ appendFile "incoming.log" $ concat ["id ",id," format ",format,
                                               "  file ",f,"\n"]

  -- Parser chooser
  parser <- case lookup format parsers of 
    Just x -> return x
    Nothing -> fail $ "Format of " ++ format ++ " is not supported"

  let info = PortfolioInfo { pId = id, tmpFile = f }
  liftIO $ parseAndSend conn (parser info) raw

  output "ok\r\n"

-- |Lookup table for parsers.
parsers = [("nordnet",nordnet)]

-- |Shorthand for failing when something is not defined.
orFail :: (Monad m) => m (Maybe a) -> String -> m a
orFail act failMsg = act >>= maybe (fail failMsg) return
