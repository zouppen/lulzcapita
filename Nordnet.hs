module Nordnet where

import Text.Parsec.Text.Lazy
import Text.Parsec
import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
import Control.Monad (liftM)
import Text.JSON

type Record = JSObject JSValue

nordnet :: Parser [Record]
nordnet = do
  -- Skipping head
  skipMany newline
  string "ID"
  skipLine
  -- The thing
  records <- many record
  -- Skip tail
  skipMany newline
  eof
  -- Suceess
  return records

-- |Parses a record. Comments are in Finnish because the CSV from
-- Nordnet has Finnish comments.
record :: Parser Record
record = do
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
  field                 -- Valuutta
  field                 -- Hankinta-arvo
  field                 -- Tulos
  skipLine
  return $ toJSObject [("_id",showJSON id)
                      ,("date",showJSON dateStr)
                      ]

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
