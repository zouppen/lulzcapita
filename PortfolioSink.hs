module Main where

import qualified Data.ByteString.Lazy as B
import Network.FastCGI

main = do
  runFastCGI portfolioSink

portfolioSink = do
  -- No error handling and stuffing everything to a file.
  Just id <- requestHeader "PortfolioID"
  Just format <- requestHeader "PortfolioFormat"
  stuff <- getBodyFPS
  liftIO $ appendFile "tavara.txt" $ "Salkku on " ++ portfolio ++ "\n"
  liftIO $ B.appendFile "tavara.txt" stuff
  output "Kissa."
