module PortfolioSink where

import Control.Monad (liftM)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.ConfigFile (ConfigParser)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TI
import Database.CouchDB
import Network.FastCGI
import System.IO
import Text.JSON
import Text.Parsec
import Text.Parsec.Text
import Common
import DatabaseTools
import Nordnet

-- |Does the actual parsing and pushing of results. Fails when
-- something goes wrong.
portfolioSink :: ConfigParser -> CGI CGIResult
portfolioSink conf = do
  -- Gets request data. 
  info <- getPortfolioHeaders conf
  -- Making Text strict to improve performance.
  raw <- liftM (decodeUtf8 . BS.concat . B.toChunks) getBodyFPS
  -- Get user ID.
  userID <- liftIO $ lulzCouch conf $ 
            getUserId info `orFail` "Your portfolio ID is not registered"

  -- Get temporary file and write the portfolio on disk for debugging.
  f <- liftIO $ do 
    (f,h) <- openBinaryTempFile "portfolio_log" ".csv"
    TI.hPutStr h raw
    hClose h
    return f

  -- Dummy access log.
  liftIO $ appendFile "portfolio.log" $ concat ["id ",pId info
                                               ," format ",format info
                                               ," file ",f,"\n"]
  
  -- Add some useful information to the documents.
  let extra = [ field "table" "portfolio"  -- Type of content
              , field "user" userID        -- Portfolio ID hash
              , field "original" f         -- Original file
              ]

  -- Parser chooser.
  p <- (return $ lookup (format info) parsers)
       `orFail` "Portfolio type is not supported"

  -- Parse the portfolio.
  parsed <- liftIO $ parsePortfolio (p info extra) raw
  
  -- Send the portfolio
  liftIO $ sendPortfolio conf parsed

  -- Write just OK, it is never actually read.
  output "ok\r\n"

field :: (JSON a) => String -> a -> (String,JSValue)
field k v = (k,showJSON v)
  
-- |Lookup table for parsers.
parsers = [("nordnet",nordnet)]

-- |Just parses and fails monadically
parsePortfolio :: (Monad m) => Parser [Record] -> Text -> m [Record]
parsePortfolio p raw =
  case parse p "portfolio" raw of
    Left e -> fail $ "Parsing of portfolio failed in " ++ show (errorPos e)
    Right a -> return a

sendPortfolio :: ConfigParser -> [Record] -> IO ()
sendPortfolio conf pf = lulzCouch conf $ mapM_ maybeUpdate pf
  where
    maybeUpdate (doc,json) = do
      ret <- newNamedDoc dbName doc json
      -- This may fail but it's not very probable. Conflight requires
      -- two simultaneous portofolio updates when it's ok anyway to
      -- ignore the other anyway.
      case ret of
        Left _ -> getAndUpdateDoc dbName doc (return . (const json))
        Right _ -> return Nothing -- Succeeded in newNamedDoc
    dbName = (peek conf "location.db")
