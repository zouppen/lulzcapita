module Main where

import Data.List (stripPrefix)
import Network.FastCGI
import Network.URI
import System.Environment
import Common
import PortfolioSink
import User

main = do
  -- TODO Migrate to cmdargs when we more parameters than one. Now we
  -- have very brutal commandline parser which dies on errors.
  [dbStr] <- getArgs
  let Just dbUri = parseURI dbStr
  
  -- Runs FastCGI and outputs exceptions as internal server
  -- errors. TODO more sophisticated messages?
  runFastCGI $ catchCGI (requestPicker dbUri) outputException

requestPicker :: URI -> CGI CGIResult
requestPicker dbUri = do
  path <- scriptName
  case stripPrefix cgiBase path of
    Just "portfolio" -> portfolioSink dbUri
    Just "userinfo" -> userInfo dbUri
    Just "register" -> output "registering is not supported yet\r\n"
    _ -> outputError 404 "Interface not found" ["Interface "++path++" does not exist"]
