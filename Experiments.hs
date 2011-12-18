module Experiments where

import Data.Text.Lazy
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
