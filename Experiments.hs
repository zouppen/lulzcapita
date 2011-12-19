module Experiments where

import Data.Text.Lazy
import Database.CouchDB
import Text.JSON
import Text.Parsec
import Common
import Nordnet

-- |Parser tester, reads portfolio from file instead of CGI.
testMe p f = do
  stuff <- readFile f -- Assumes UTF-8
  parsed <- parsePortfolio (p dummyInfo) (pack stuff)
  mapM_ (putStrLn.encode) parsed
  where dummyInfo = PortfolioInfo { pId = "1337", tmpFile = f }

-- |Database sending tester.
sendMe p id f = do
  stuff <- readFile f -- Assumes UTF-8
  conn <- createCouchConn "localhost" 5984
  parseAndSend conn (p dummyInfo) (pack stuff)
  where dummyInfo = PortfolioInfo { pId = id, tmpFile = f }
