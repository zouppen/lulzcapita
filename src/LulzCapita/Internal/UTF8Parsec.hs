-- |Simple UTF-8 aware Parsec Stream instance. Uncompatible with
-- normal ByteString parser because exports the same instance.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LulzCapita.Internal.UTF8Parsec
    ( Parser, GenParser, UTF8String(..)
    ) where

import qualified Codec.Binary.UTF8.Generic as U
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)
import Text.Parsec.Error
import Text.Parsec.Prim

-- |"New" string type to allow overlapping ByteString instances.
data UTF8String = UTF8String ByteString  deriving (Show)

instance (Monad m) => Stream UTF8String m Char where
    uncons (UTF8String a) = return $ do 
      (c,bs) <- U.uncons a
      return (c,UTF8String bs)
    {-# INLINE uncons #-}

type Parser = Parsec UTF8String ()
type GenParser st = Parsec UTF8String st
