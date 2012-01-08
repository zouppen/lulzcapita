{-# LANGUAGE RecordWildCards #-}
module LulzCapita.Bank.Nordnet (nordnet) where

import Text.Parsec
import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
import Database.CouchDB
import Control.Monad (liftM)
import Text.JSON
import LulzCapita.Common
import LulzCapita.Internal.UTF8Parsec

nordnet :: PortfolioInfo -> Parser [Transaction]
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
record :: PortfolioInfo -> Parser Transaction
record PortfolioInfo{..} = do
  -- Stop if the line is empty.
  lookAhead $ satisfy (/= '\n')
  
  -- Start reading the fields.
  id <- field           -- ID
  field                 -- Kirjauspäivä
  dateStr <- field      -- Kauppapäivä
  field                 -- Maksupäivä
  taStr <- field        -- Tapahtumatyyppi
  symbol <- field       -- Arvopaperi
  stockType <- field    -- Instrumenttityyppi
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
        "LÄHDEVERO"        -> ("tax",False,Nothing) -- Tax of savings interest.
        "ENNAKKOPIDÄTYS"   -> ("tax",True,Nothing)  -- Tax of dividents.
        "OSTO"             -> ("sale",True,Just ("count",showJSON amount))
        "MYYNTI"           -> ("sale",True,Just ("count",showJSON (-amount)))
        "TALLETUSKORKO"    -> ("income",False,Nothing)
        "PÄÄOMAN PALAUTUS" -> ("income",True,Nothing)
        "OSINKO"           -> ("income",True,Nothing)
        -- Some "pathological" transactions:
        -- Extra stocks (Nordnet welcome offer, bonus issue)
        "JÄTTÖ SIIRTO"         -> ("sale",True,Just ("count",showJSON amount))
        "RAHASTOANTI AP JÄTTÖ" -> ("sale",True,Just ("count",showJSON amount))
        -- Transactions related to cancellation of dividents, taxes, etc.
        "OSINGON PERUUTUS"     -> ("income",True,Nothing)
        "PER ULK KUPONKIVERO"  -> ("tax",True,Nothing)
        -- Unknown events are logged, too.
        a -> ("unknown",False,Just ("unsupported",showJSON a))
        
  let result = (hash [id],
                catMaybes [Just ("date",showJSON dateStr)
                          ,Just ("type",showJSON typ)
                          ,if showIsin then Just ("isin",showJSON isin) else Nothing
                          ,Just ("sum",showJSON euros)
                          ,ta
                          ])
  
  let rawBlob = toJSObject [("symbol", symbol),("type", stockType)]
  let stockInfo = ("isin_"++isin,[("raw",showJSON rawBlob)])

  return $ Transaction result stockInfo

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
