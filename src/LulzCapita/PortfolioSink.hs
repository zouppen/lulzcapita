{-# LANGUAGE RecordWildCards #-}

-- |Functions related to portfolio uploading. Only "generic" tools
-- here. Bank specific parsers are in separate modules.
module LulzCapita.PortfolioSink where

import qualified Data.ByteString.Lazy as B
import Data.ConfigFile (ConfigParser)
import Data.DateTime (getCurrentTime, toSeconds)
import Database.CouchDB
import Network.FastCGI
import System.IO
import Text.JSON
import Text.Parsec
import LulzCapita.Internal.UTF8Parsec
import LulzCapita.Common
import LulzCapita.DatabaseTools
import LulzCapita.Bank.Nordnet

-- |Does the actual parsing and pushing of results. Fails when
-- something goes wrong.
portfolioSink :: ConfigParser -> CGI CGIResult
portfolioSink conf = do
  -- Gets request data. 
  info <- getPortfolioHeaders conf
  -- Making Text strict to improve performance.
  raw <- getBodyFPS

  -- Get temporary file and write the portfolio on disk for debugging.
  f <- liftIO $ do 
    (f,h) <- openBinaryTempFile "portfolio_log" ".csv"
    B.hPutStr h raw
    hClose h
    return f

  -- Dummy access log.
  liftIO $ appendFile "portfolio.log" $ concat ["id ",pId info
                                               ," format ",format info
                                               ," file ",f,"\n"]
  
  -- Get user ID and write synchronization info.
  (userID,syncID) <- lulzCouch conf $ do
      userID <- getUserId info `orFail` "Your portfolio ID is not registered"
      syncID <- putSyncInfo info f
      return (userID,syncID)

  -- Add some useful information to the documents.
  let extra = [ field "table" "portfolio"  -- Type of content
              , field "user" userID        -- Portfolio ID hash
              , field "sync" syncID        -- Synchronization ID
              ]

  -- Parser chooser.
  p <- (return $ lookup (format info) parsers)
       `orFail` "Portfolio type is not supported"

  -- Parse and send the portfolio.
  xs <- liftIO $ parsePortfolio (p info) (UTF8String raw)
  let actions = map ((recordToJson extra) . securityAction) xs
  let infos = map ((recordToJson [field "table" "security"]) . securityInfo) xs
  liftIO $ sendPortfolio conf actions
  liftIO $ sendInfo conf infos

  -- Write just OK, it is never actually read.
  logCGI $ "User " ++ show userID ++ " synchronization " ++ show syncID
  output "ok\r\n"

field :: (JSON a) => String -> a -> (String,JSValue)
field k v = (k,showJSON v)
  
-- |Lookup table for parsers.
parsers = [("nordnet",nordnet)]

-- |Just parses and fails monadically
parsePortfolio :: (Monad m) => Parser [Transaction] -> UTF8String -> m [Transaction]
parsePortfolio p raw =
  case parse p "portfolio" raw of
    Left e -> fail $ "Parsing of portfolio failed in " ++ show (errorPos e)
    Right a -> return a

sendPortfolio :: ConfigParser -> [(Doc,JSObject JSValue)] -> IO ()
sendPortfolio conf pf = lulzCouch conf $ mapM_ maybeUpdate pf
  where
    maybeUpdate (doc,json) = do
      ret <- newNamedDoc dbName doc json
      -- Check if the document is already there. Resending it with
      -- getAndUpdate in case the numbers have changed from previous
      -- sync. Even the update may fail but it happens only if a user
      -- is simultaneously syncing the very same portfolio.
      case ret of
        Left _ -> getAndUpdateDoc dbName doc (return . (const json))
        Right _ -> return Nothing -- Succeeded in newNamedDoc
    dbName = (peek conf "location.db")

-- |Sends simple information about securities information if there is
-- |no such information yet. Ignore if there is one yet.
sendInfo :: ConfigParser -> [(Doc,JSObject JSValue)] -> IO ()
sendInfo conf infos = lulzCouch conf $ mapM_ tryWrite infos
  where tryWrite (doc,json) = newNamedDoc dbName doc json
        dbName = (peek conf "location.db")

-- |Puts synchronization log to the database.
putSyncInfo :: PortfolioInfo -> FilePath -> CouchMonad Doc
putSyncInfo PortfolioInfo{..} logF = do
  time <- liftIO getCurrentTime
  (doc,_) <- newDoc (peek conf "location.db") $ toJSObject
    [ field "table" "sync"
    , field "portfolio" $ hash []
    , field "time" $ toSeconds time
    , field "log" logF
    ]
  return doc

-- |Adds extra fields and converts object to real JSON value.
recordToJson :: Fields -> Record -> (Doc,JSObject JSValue)
recordToJson extra (key,fields) = (doc key,toJSObject (fields++extra))
