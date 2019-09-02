{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Ledgit.Data where

import           Data.Csv
import           Data.Text     as T
import           Data.Time
import           Protolude

-- | Header for Commerzbank csv files
--   there are some weird characters at the beginning of the header
commerzbankHeader :: Text
commerzbankHeader = "Transaction date,Value date,Transaction type,Booking text,Amount,Currency,Account of initiator,Bank code of account of initiator,IBAN of account of initiator,Category"

-- | Header for N26 csv files
n26Header :: Text
n26Header = "Date,Payee,Account number,Transaction type,Payment reference,Category,Amount (EUR), Amount (Foreign Currency),Type Foreign Currency,Exchange Rate"

-- | Header for Revolue csv files
revolutHeader :: Text
revolutHeader = "Completed Date ; Description ; Paid Out (EUR) ; Paid In (EUR) ; Exchange Out; Exchange In; Balance (EUR); Category; Notes"

-- | Header for YNAB csv files
ynabHeader :: Header
ynabHeader = header ["Date", "Payee", "Memo", "Amount"]

-- | Possible input lines
data InputLedgerLine =
    CommerzbankLine LedgerLine
  | N26Line LedgerLine
  | RevolutLine LedgerLine
  deriving (Eq, Show)

-- | Normalize input lines
toLedgerLine :: InputLedgerLine -> LedgerLine
toLedgerLine (CommerzbankLine l) = l
toLedgerLine (N26Line l)         = l
toLedgerLine (RevolutLine l)     = l

-- | Representation of a line in the file
data LedgerLine = LedgerLine {
  _date      :: Maybe Day
, _amount    :: Amount
, _reference :: Reference
, _category  :: Maybe Category
} deriving (Eq, Show)

setTodaysDateIfMissing :: (MonadIO m) => LedgerLine -> m LedgerLine
setTodaysDateIfMissing line = liftIO $
  case _date line of
    Just _ -> pure line
    Nothing -> do
      today <- utctDay <$> getCurrentTime
      pure line { _date = Just today }

makeDecodeOptions :: Text -> DecodeOptions
makeDecodeOptions fileName =
  defaultDecodeOptions {
    decDelimiter = fromIntegral (ord (csvSeparator fileName))
  }

csvSeparator :: Text -> Char
csvSeparator fileName =
  if T.isInfixOf "Revolut" fileName
  then ';'
  else ','

instance FromNamedRecord InputLedgerLine where
  parseNamedRecord r =
        parseCommerzBank
    <|> parseN26
    <|> parseRevolut

    where
      parseCommerzBank = fmap CommerzbankLine $
            LedgerLine
        <$> (fmap unCommerzbankDay <$> r .: "Transaction date")
        <*> r .: "Amount"
        <*> r .: "Booking text"
        <*> r .: "Category"

      parseN26 = fmap N26Line $
            LedgerLine
        <$> (fmap unN26Day <$> r .: "Date")
        <*> r .: "Amount (EUR)"
        <*> r .: "Payee"
        <*> r .: "Category"

      parseRevolut = fmap RevolutLine $
            LedgerLine
        <$> (fmap unRevolutDay <$> r .: "Completed Date ")
        <*> ((negate <$> r .: " Paid Out (EUR) ") <|> r .: " Paid In (EUR) ")
        <*> r .: " Description "
        <*> r .: " Category"

instance ToNamedRecord LedgerLine where
  toNamedRecord (LedgerLine date amount reference category) =
    namedRecord [
       "Date"   .= date
     , "Amount" .= amount
     , "Payee"  .= reference
     , "Memo"   .= category
     ]

newtype Category = Category Text
  deriving (Eq, Show, IsString)
  deriving newtype FromField
  deriving newtype ToField

newtype Reference = Reference Text
  deriving (Eq, Show, IsString)
  deriving newtype FromField
  deriving newtype ToField

newtype Amount = Amount Double
  deriving (Eq, Show)
  deriving newtype FromField
  deriving newtype ToField
  deriving newtype Num

newtype CommerzbankDay = CommerzbankDay { unCommerzbankDay :: Day } deriving (Eq, Show)
newtype N26Day         = N26Day { unN26Day :: Day } deriving (Eq, Show)
newtype RevolutDay     = RevolutDay { unRevolutDay :: Day } deriving (Eq, Show)

instance FromField CommerzbankDay where
  parseField f = CommerzbankDay <$>
    parseTimeM True defaultTimeLocale "%d.%m.%Y" (toS f)

instance FromField N26Day where
  parseField f = N26Day <$>
    parseTimeM True defaultTimeLocale "%Y-%m-%d" (toS f)

instance FromField RevolutDay where
  parseField f = RevolutDay <$>
    parseTimeM True defaultTimeLocale "%e %h %Y" (toS f)

instance ToField Day where
  toField f =
    toS $ formatTime defaultTimeLocale "%Y-%m-%d" f

data CliOptions = CliOptions {
  inputFile  :: Text
, outputFile :: Maybe Text
} deriving (Eq, Show)
