{-# LANGUAGE RecordWildCards #-}
module DatabaseTools where

import Data.ConfigFile (ConfigParser)
import Database.CouchDB
import Network.FastCGI
import Text.JSON
import Common

-- |Shortcut for running something on Lulzcapita database.
lulzCouch :: (MonadIO m) => ConfigParser -> CouchMonad a -> m a
lulzCouch conf act = liftIO $ runCouchDBURI (peek conf "secret.db_uri") act

-- |Digs portfolio information from FastCGI request.
getPortfolioHeaders :: ConfigParser -> CGI PortfolioInfo
getPortfolioHeaders conf = do
  -- Getting the stuff from a request. 
  pId <- requestHeader "PortfolioID" `orFail` "Portfolio ID is not defined"
  format <- requestHeader "PortfolioFormat" `orFail` "Portfolio format is not defined"
  let hash = \x -> hashGen conf (format:pId:x)
  return $ PortfolioInfo {..}

-- |Returns user ID of given portfolio or Nothing if not found.
getUserId :: PortfolioInfo -> CouchMonad (Maybe Doc)
getUserId PortfolioInfo{..} = do
  -- Find relevant user ID.
  userRaw <- queryView (peek conf "location.db") (doc "couchapp")
             (doc "portfolio_user") [("key",showJSON $ hash [])]

  -- Dig the document ID out by heavy pattern matching.
  return $ case userRaw :: [(Doc,JSValue)] of
    [(doc,_)] -> Just doc
    _         -> Nothing

