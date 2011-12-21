module Common where

import Database.CouchDB
import Text.JSON

-- Base URL for FastCGI
cgiBase = "/bin/"

-- Database names
portfolioDb = db "capita-portfolio"


-- Some information to pass to parser.
data PortfolioInfo = PortfolioInfo { pId     :: String
                                   , tmpFile :: FilePath
                                   } deriving (Show)

type Record = (Doc,JSObject JSValue)

-- |Shorthand for failing when something is not defined.
orFail :: (Monad m) => m (Maybe a) -> String -> m a
orFail act failMsg = act >>= maybe (fail failMsg) return
