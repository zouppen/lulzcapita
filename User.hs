{-# LANGUAGE RecordWildCards #-}

-- |This dumps user info for Greasemonkey script. This provides very
-- much "Nordnet only" data at the moment.
module User where

import Data.ConfigFile (ConfigParser)
import Database.CouchDB
import Network.FastCGI
import Text.JSON
import Common
import DatabaseTools

userInfo :: ConfigParser -> CGI CGIResult
userInfo conf = do
  info <- getPortfolioHeaders conf 
  json <- lulzCouch conf $ readDB info
  output $ encode json

-- |Extracts the information from databases. The portfolio parameter
-- should be hashed.
readDB :: PortfolioInfo -> CouchMonad (JSObject JSValue)
readDB PortfolioInfo{..} = do
  -- Should return only one row.
  timeRaw <- queryView (peek conf "location.db")
             (doc "couchapp") (doc "last_sync")
             [("key",jsonHash)
             ,("group",showJSON True)
             ]

  -- If there is some data, return it, otherwise the default
  let (isFirst,timestamp) = case timeRaw of
        [(_,x)] -> (False,x)
        []      -> (True,showJSON "2000-01-01")  -- Start of the time.

  -- Get nick of that user.
  userRaw <- queryView (peek conf "location.db")
             (doc "couchapp") (doc "portfolio_user")
             [("key",jsonHash)]

  -- If there is some data, return it, otherwise the default.
  userDoc <- case userRaw :: [(Doc,JSValue)] of
    [(doc,_)] -> do
      logCGI $ "User " ++ show doc ++ " requests info"
      getDoc (peek conf "location.db") doc
    [] -> do
      logCGI $ "Unknown user looking for portfolio " ++ hash []
      return Nothing
    
  -- Now get the doc name, putting it to returned value.
  let userjson = case userDoc of
        Just (_,_,a) -> a
        Nothing      -> JSNull

  return $ toJSObject [("last",timestamp)
                      ,("is_first",showJSON isFirst)
                      ,("user",userjson)
                      ]

  where jsonHash = showJSON $ hash []
