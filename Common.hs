module Common where

import Prelude hiding (concat)
import Data.ByteString.Lazy.Char8 (intercalate,pack,singleton)
import Data.Digest.Pure.SHA
import Database.CouchDB
import Text.JSON

-- Base URL for FastCGI
cgiBase = "/bin/"

-- Database names
portfolioDb = db "capita-portfolio"

-- |Server secret value for hashing the original IDs. A kind of salt value.
codingKey = pack "kissa"

-- Some information to pass to parser.
data PortfolioInfo = PortfolioInfo { pId     :: String
                                   , tmpFile :: FilePath
                                   } deriving (Show)

type Record = (Doc,JSObject JSValue)

-- |Shorthand for failing when something is not defined.
orFail :: (Monad m) => m (Maybe a) -> String -> m a
orFail act failMsg = act >>= maybe (fail failMsg) return

-- |Converts portfolio type and ID to hashed value. Useful in hiding
-- the original IDs in the public database. I suppose there is nothing
-- secret with those values but I want to play safe. The converter
-- avoids collisions with a cryptographic hash, which is truncated to
-- 16 bytes (32 hexadecimal digits) to make it look like CouchDB
-- generated ID.
hash :: [String] -> String
hash names = take 32 $ showDigest $ sha256 $
             intercalate (singleton '\NUL') (codingKey:map pack names)
