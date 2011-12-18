module Common where

import Data.Text.Lazy
import Database.CouchDB
import Text.JSON
import Text.Parsec
import Text.Parsec.Text.Lazy

-- Some information to pass to parser.
data PortfolioInfo = PortfolioInfo { pId     :: String
                                   , tmpFile :: FilePath
                                   } deriving (Show)

type Record = JSObject JSValue

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
  mapM_ (newDoc (db "lulzcapita")) pf
