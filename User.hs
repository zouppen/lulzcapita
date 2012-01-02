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
readDB info@PortfolioInfo{..} = do
  -- Should return only one row.
  timeRaw <- queryView (peek conf "location.db")
             (doc "couchapp") (doc "last_sync")
             [("key",jsonHash)
             ,("group",showJSON True)
             ]

  -- If there is sync date, return it, otherwise the default.
  let (isFirst,timestamp) = case timeRaw of
        [(_,x)] -> (False,x)
        []      -> (True,showJSON (946728000::Int))  -- Start of the time, Y2K.

  -- Get nick of that user.
  mbUser <- getUserId info
  userDoc <- case mbUser of
    Just user -> do
      logCGI $ "User " ++ show user ++ " requests info"
      -- Now getting the actual user doc. The pattern matching is
      -- considered "safe" because otherwise database is inconsistent.
      Just (_,_,a) <- getDoc (peek conf "location.db") user
      return a
    Nothing -> do
      logCGI $ "Unknown user looking for portfolio " ++ hash []
      return JSNull

  return $ toJSObject [("last",timestamp)
                      ,("is_first",showJSON isFirst)
                      ,("user",userDoc)
                      ]

  where jsonHash = showJSON $ hash []
