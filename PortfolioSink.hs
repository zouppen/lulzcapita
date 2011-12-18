module Main where

import Control.Monad (liftM,unless)
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.IO as TI
import Network.FastCGI
import System.IO
import Text.JSON
import Text.Parsec
import Common
import Nordnet

main = do
  runFastCGI tryPortfolioSink

-- |Runs FastCGI and outputs exceptions as internal server
-- errors. TODO more sophisticated messages?
tryPortfolioSink :: CGI CGIResult
tryPortfolioSink = catchCGI portfolioSink outputException

-- |Does the actual parsing and pushing of results. Fails when
-- something goes wrong.
portfolioSink :: CGI CGIResult
portfolioSink = do
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

  -- Parser chooser
  parser <- case lookup format parsers of 
    Just x -> return x
    Nothing -> fail $ "Format of " ++ format ++ " is not supported"

  let info = PortfolioInfo { pId = id, tmpFile = f }

  -- Parsing
  portfolio <- case parse (parser info) "portfolio" raw of
    Left e -> fail $ "Parsing of portfolio failed in " ++ show (errorPos e)
    Right a -> return a 

  liftIO $ appendFile "portfoliot.txt" $ show portfolio

  output "ok\r\n"

-- |Lookup table for parsers.
parsers = [("nordnet",nordnet)]

-- |Shorthand for failing when something is not defined.
orFail :: (Monad m) => m (Maybe a) -> String -> m a
orFail act failMsg = act >>= maybe (fail failMsg) return
