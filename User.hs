module User where

import Data.ConfigFile (ConfigParser)
import Database.CouchDB
import Network.FastCGI
import Text.JSON
import Common

userInfo :: ConfigParser -> CGI CGIResult
userInfo conf = do
  id <- requestHeader "PortfolioID" `orFail` "Portfolio ID is not defined"
  format <- requestHeader "PortfolioFormat" `orFail` "Portfolio format is not defined"
  
  json <- liftIO $ runCouchDBURI (peek conf "secret.db") $
          readDB conf $ hash conf [format,id]
  output $ encode json

readDB :: ConfigParser -> String -> CouchMonad (JSObject JSValue)
readDB conf portfolio = do
  -- Should return only one row.
  timeRaw <- queryView (peek conf "database_names.portfolio")
             (doc "couchapp") (doc "lastsync")
             [("key",showJSON portfolio)
             ,("group",showJSON True)
             ]
  
  -- If there is some data, return it, otherwise the default
  let timestamp = case timeRaw of
        [(_,x)] -> x
        [] -> showJSON "2000-01-01"  -- Start of the time

  return $ toJSObject [("last",timestamp)]
