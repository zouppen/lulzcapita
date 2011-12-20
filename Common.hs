module Common where

import Data.Text.Lazy
import Database.CouchDB
import Text.JSON
import Text.Parsec
import Text.Parsec.Text.Lazy

portfolioDb = db "lulzcapita"

-- Some information to pass to parser.
data PortfolioInfo = PortfolioInfo { pId     :: String
                                   , tmpFile :: FilePath
                                   } deriving (Show)

type Record = (Doc,JSObject JSValue)

parseAndSend :: CouchConn -> Parser [Record] -> Text -> IO ()
parseAndSend conn p raw = parsePortfolio p raw >>= sendPortfolio conn

-- |Just parses and fails monadically
parsePortfolio :: (Monad m) => Parser [Record] -> Text -> m [Record]
parsePortfolio p raw =
  case parse p "portfolio" raw of
    Left e -> fail $ "Parsing of portfolio failed in " ++ show (errorPos e)
    Right a -> return a

sendPortfolio :: CouchConn -> [Record] -> IO ()
sendPortfolio conn pf = runCouchDBWith conn $ do
  mapM_ maybeUpdate pf
  where
    maybeUpdate (doc,json) = do
      ret <- newNamedDoc portfolioDb doc json
      -- This may fail but it's not very probable. Conflight requires
      -- two simultanous portofolio updates when it's ok anyway to 
      -- ignore the other anyway.
      case ret of
        Left _ -> getAndUpdateDoc portfolioDb doc (return . (const json))
        Right _ -> return Nothing -- Not interested
