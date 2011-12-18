module Main where

import Control.Monad (liftM,unless)
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.FastCGI
import Text.Parsec
import Text.Parsec.Text.Lazy
import Text.JSON
import Data.Text.Lazy
import Nordnet

main = do
  runFastCGI tryPortfolioSink

-- |Runs FastCGI and outputs exceptions as internal server
-- errors. TODO more sophisticated messages?
tryPortfolioSink :: CGI CGIResult
tryPortfolioSink = catchCGI portfolioSink outputException

portfolioSink :: CGI CGIResult
portfolioSink = do
  -- Getting the stuff from a request
  id <- requestHeader "PortfolioID" `orFail` "Portfolio ID is not defined"
  format <- requestHeader "PortfolioFormat" `orFail` "Portfolio format is not defined"
  raw <- liftM decodeUtf8 getBodyFPS
  
  -- Parser chooser
  parser <- case lookup format parsers of 
    Just x -> return x
    Nothing -> fail $ "Format of " ++ format ++ " is not supported"

  -- Parsing
  portfolio <- case parse parser "portfolio" raw of
    Left e -> fail $ "Parsing of portfolio failed in " ++ show (errorPos e)
    Right a -> return a 

  liftIO $ appendFile "portfoliot.txt" $ show portfolio

  output "ok\r\n"

parsers = [("nordnet",nordnet)]

orFail :: (Monad m) => m (Maybe a) -> String -> m a
orFail act failMsg = act >>= maybe (fail failMsg) return

-- |Parser tester, reads portfolio from file instead of CGI.
testMe p f = do
  stuff <- readFile f -- Assumes UTF-8
  case parse p "portfolio" (pack stuff) of
    Left e -> putStrLn $ "broken: " ++ show e
    Right a -> mapM_ (putStrLn.encode) a
