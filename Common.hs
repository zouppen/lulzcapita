module Common where

import Data.ByteString.Lazy.Char8 (ByteString,intercalate,pack,singleton)
import Data.ConfigFile (Get_C, ConfigParser, SectionSpec, OptionSpec, get)
import Data.Digest.Pure.SHA
import Data.Either.Utils (forceEither)
import Database.CouchDB
import Text.JSON
import Network.URI (URI,parseURI)

-- Adding ability to parse URIs
instance Get_C URI where
  get a b c = do 
    s <- get a b c
    case parseURI s of
      Just x -> return x
      Nothing -> fail "Not a valid URI"

-- ByteString is parsed like string but packed.
instance Get_C ByteString where
  get a b c = get a b c >>= return . pack

-- Database name is like string, but wrapped.
instance Get_C DB where
  get a b c = get a b c >>= return . db

-- Some information to pass to parser.
data PortfolioInfo = PortfolioInfo { pId     :: String
                                   , tmpFile :: FilePath
                                   , conf    :: ConfigParser
                                   }

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
hash :: ConfigParser -> [String] -> String
hash conf names = take 32 $ showDigest $ sha256 $
                  intercalate (singleton '\NUL')
                  (peek conf "secret.salt":map pack names)

-- |Just unwraps the value in Either and allows specifying config
-- |entry with dot notation (secret.db)
peek :: (Get_C a) => ConfigParser -> String -> a
peek conf combi = forceEither $ get conf section option
  where (section,(_:option)) = span (/='.') combi
