module Main where

import Control.Monad (liftM, when)
import Data.ConfigFile
import Data.Either.Utils
import Data.List (stripPrefix)
import Network.FastCGI
import Network.URI
import LulzCapita.Common
import LulzCapita.PortfolioSink
import LulzCapita.User

main = do
  -- TODO Add some config file validation and allow specifying config
  -- file location. Also, check the values for sanity before going forward.
  conf <- liftM forceEither $ readfile emptyCP "sink.conf"
  when (peek conf "secret.salt" == "change me") $
    fail "Please generate secret.salt with pwgen or similar tool."

  -- Runs FastCGI and outputs exceptions as internal server
  -- errors. TODO more sophisticated messages?
  runFastCGI $ catchCGI (requestPicker conf) outputException

requestPicker :: ConfigParser -> CGI CGIResult
requestPicker conf = do
  path <- scriptName
  case stripPrefix (peek conf "location.base_url") path of
    Just "portfolio" -> portfolioSink conf
    Just "userinfo" -> userInfo conf
    Just "register" -> output "registering is not supported yet\r\n"
    _ -> outputError 404 "Interface not found" ["Interface "++path++" does not exist"]
