{-# LANGUAGE RecordWildCards #-}
module Nordnet (nordnet) where

import Text.Parsec.Text
import Text.Parsec
import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
import Database.CouchDB
import Control.Monad (liftM)
import Text.JSON
import Common

nordnet :: PortfolioInfo -> Parser [Record]
nordnet info = do
  -- Skipping head
  skipMany newline
  string "ID"
  skipLine
  -- The thing
  records <- many $ record info
  -- Skip tail
  skipMany newline
  eof
  -- Suceess
  return records

-- |Parses a record. Comments are in Finnish because the CSV from
-- Nordnet has Finnish comments.
record :: PortfolioInfo -> Parser Record
record PortfolioInfo{..} = do
  -- Stop if the line is empty.
  lookAhead $ satisfy (/= '\n')
  
  -- Start reading the fields.
  id <- field           -- ID
  field                 -- Kirjauspäivä
  dateStr <- field      -- Kauppapäivä
  field                 -- Maksupäivä
  taStr <- field        -- Tapahtumatyyppi
  field                 -- Arvopaperi
  field                 -- Instrumenttityyppi
  isin <- field         -- ISIN
  amount <- numberField -- Määrä
  field                 -- Kurssi
  field                 -- Korko
  field                 -- Maksut
  euros <- numberField  -- Summa
  skipLine -- There are some more fields, but nothing interesting.
  
  -- Defining transaction specific object.
  let (typ,showIsin,ta) = case taStr of
        "TALLETUS"         -> ("account",False,Nothing)
        "NOSTO"            -> ("account",False,Nothing)
        "LÄHDEVERO"        -> ("tax",True,Nothing)
        "ENNAKKOPIDÄTYS"   -> ("tax",True,Nothing)
        "OSTO"             -> ("sale",True,Just ("count",showJSON amount))
        "MYYNTI"           -> ("sale",True,Just ("count",showJSON (-amount)))
        "TALLETUSKORKO"    -> ("income",True,Nothing)
        "PÄÄOMAN PALAUTUS" -> ("income",True,Nothing)
        "OSINKO"           -> ("income",True,Nothing)
        -- Some "pathological" transactions:
        -- Extra stocks (Nordnet welcome offer, bonus issue)
        "JÄTTÖ SIIRTO"         -> ("sale",True,Just ("count",showJSON amount))
        "RAHASTOANTI AP JÄTTÖ" -> ("sale",True,Just ("count",showJSON amount))
        -- Transactions related to cancellation of dividents, etc.
        "OSINGON PERUUTUS"     -> ("income",True,Nothing)
        "PER ULK KUPONKIVERO"  -> ("income",True,Nothing)
        -- Unknown events are logged, too.
        a -> ("unknown",False,Just ("unsupported",showJSON a))
        
  return ((doc $ hash ["nordnet",pId,id]),toJSObject $ catMaybes
    [Just ("portfolio", showJSON $ hash ["nordnet",pId])
    ,Just ("original", showJSON tmpFile)
    ,Just ("date",showJSON dateStr)
    ,Just ("type",showJSON typ)
    ,if showIsin then Just ("isin",showJSON isin) else Nothing
    ,Just ("sum",showJSON euros)
    ,ta
    ])

obj k v = (k,toJSObject v)

field :: Parser String
field = manyTill anyChar tab

-- |Number, comma as decimal separator and "fancy" spaces as thousand
-- separators. Somewhat ugly because dot is so hard-wired to Haskell.
numberField :: Parser Double
numberField = do
  sign <- optionMaybe $ char '-'
  raw <- many1 $ choice [stupidComma,stupidSpace,stupidDigits]
  tab
  return $ read $ catMaybes $ sign:raw
  where stupidComma = char ',' >> return (Just '.')
        stupidSpace = char ' ' >> return Nothing
        stupidDigits = liftM Just digit

skipLine :: Parser ()
skipLine = do
  manyTill anyChar newline
  return ()
