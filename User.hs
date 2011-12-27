{-# LANGUAGE RecordWildCards #-}
module User where

import Data.ConfigFile (ConfigParser)
import Database.CouchDB
import Network.FastCGI
import Text.JSON
import Common

userInfo :: ConfigParser -> CGI CGIResult
userInfo conf = do
  info <- getPortfolioHeaders conf 
  json <- liftIO $ runCouchDBURI (peek conf "secret.db") $ readDB info
  output $ encode json

-- |Extracts the information from databases. The portfolio parameter
-- should be hashed.
readDB :: PortfolioInfo -> CouchMonad (JSObject JSValue)
readDB PortfolioInfo{..} = do
  -- Should return only one row.
  timeRaw <- queryView (peek conf "location.db")
             (doc "couchapp") (doc "lastsync")
             [("key",jsonHash)
             ,("group",showJSON True)
             ]

  -- If there is some data, return it, otherwise the default
  let timestamp = case timeRaw of
        [(_,x)] -> x
        []      -> showJSON "2000-01-01"  -- Start of the time

  -- Get nick of that user.
  userRaw <- queryView (peek conf "database_names.user")
             (doc "couchapp") (doc "portfolio")
             [("key",jsonHash)]

  logCGI $ show jsonHash ++ " " ++ show userRaw -- Miksei löydy....

  -- If there is some data, return it, otherwise the default.
  userDoc <- case userRaw :: [(Doc,JSValue)] of
    [(doc,_)] -> getDoc (peek conf "database_names.user") doc
    []        -> return Nothing
    
  -- Now get the doc name, putting it to returned value.
  let userjson = case userDoc of
        Just (_,_,a) -> a
        Nothing      -> JSNull

  return $ toJSObject [("last",timestamp)
                      ,("user",userjson)
                      ]

  where jsonHash = showJSON $ hash []
