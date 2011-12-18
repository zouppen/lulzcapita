module Experiments where

import Data.Text.Lazy
import Text.JSON
import Text.Parsec
import Common
import Nordnet

-- |Parser tester, reads portfolio from file instead of CGI.
testMe p f = do
  stuff <- readFile f -- Assumes UTF-8
  case parse (p dummyInfo) "portfolio" (pack stuff) of
    Left e -> putStrLn $ "broken: " ++ show e
    Right a -> mapM_ (putStrLn.encode) a
  where dummyInfo = PortfolioInfo { pId = "1337", tmpFile = f }
