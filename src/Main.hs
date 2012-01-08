module Main where

import Control.Monad (liftM, when)
import Data.ConfigFile
import Data.Either.Utils
import Data.List (stripPrefix)
import LulzCapita.Common
import LulzCapita.PortfolioSink
import LulzCapita.User
import Network.FastCGI
import Network.URI
import System.Environment (getArgs)

main = do
  -- Very lightweight argument parser.
  args <- getArgs
  let confFile = case args of
        [file] -> file
        [] -> "lulzcapita.conf"
        _ -> error "Errorneous parameters"
  
  -- Reads config file and does some simple validation.
  conf <- liftM forceEither $ readfile emptyCP confFile
  when (peek conf "secret.salt" == "change me") $
    fail "Please generate secret.salt with pwgen or similar tool."

  -- Runs FastCGI and outputs exceptions as internal server
  -- errors. TODO more sophisticated error messages?
  runFastCGI $ catchCGI (requestPicker conf) outputException

requestPicker :: ConfigParser -> CGI CGIResult
requestPicker conf = do
  path <- scriptName
  case stripPrefix (peek conf "location.base_url") path of
    Just "portfolio" -> portfolioSink conf
    Just "userinfo" -> userInfo conf
    Just "register" -> output "registering is not supported yet\r\n"
    _ -> outputError 404 "Interface not found" ["Interface "++path++
                                                " does not exist"]
