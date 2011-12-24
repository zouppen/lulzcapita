module User where

import Database.CouchDB
import Network.FastCGI
import Network.URI
import Text.JSON
import Common

userInfo :: URI -> CGI CGIResult
userInfo uri = do
  id <- requestHeader "PortfolioID" `orFail` "Portfolio ID is not defined"
  format <- requestHeader "PortfolioFormat" `orFail` "Portfolio format is not defined"
  
  json <- liftIO $ runCouchDBURI uri $ readDB $ hash [format,id]
  output $ encode json

readDB :: String -> CouchMonad (JSObject JSValue)
readDB portfolio = do
  -- Should return only one row.
  timeRaw <- queryView portfolioDb (doc "couchapp") (doc "lastsync")
             [("key",showJSON portfolio)
             ,("group",showJSON True)
             ]
  
  -- If there is some data, return it, otherwise the default
  let timestamp = case timeRaw of
        [(_,x)] -> x
        [] -> showJSON "2000-01-01"  -- Start of the time

  return $ toJSObject [("last",timestamp)]
