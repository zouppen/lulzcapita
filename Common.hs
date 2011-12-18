module Common where

-- Some information to pass to parser.
data PortfolioInfo = PortfolioInfo { pId     :: String
                                   , tmpFile :: FilePath
                                   } deriving (Show)
