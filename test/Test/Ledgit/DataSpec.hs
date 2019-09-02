{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Test.Ledgit.DataSpec where

import           Data.Csv             hiding (header)
import           Data.Text            hiding (head)
import           Data.Time.Calendar
import           Ledgit.Data
import           Protolude
import           Test.Tasty.Hedgehogx


test_parse_commerzbank = test "we can parse the commerzbank format" $ do
  let line = ",,,\"Rundfunk ARD,xxx\",-52.50,EUR,632322701,12040000,DE92120400000632322701,"
  decodeCommerzbank line $ \result ->
    result === Just (CommerzbankLine $ LedgerLine {
         _reference = "Rundfunk ARD,xxx"
       , _date = Nothing
       , _amount = Amount (-52.50)
       , _category = Nothing
       })

test_parse_commerzbank_with_date = test "we can parse the commerzbank format with a date" $ do
  let line = "30.08.2019,30.08.2019,debit,\"mobilcom-debitel Kd\",-15.00,EUR,632322701,12040000,DE92120400000632322701,Home Phone and Internet"
  decodeCommerzbank line $ \result ->
    result === Just (CommerzbankLine $ LedgerLine {
        _reference = "mobilcom-debitel Kd"
      , _date = Just (fromGregorian 2019 8 30)
      , _amount = Amount (-15.0)
      , _category = Just ("Home Phone and Internet")
      })

test_parse_n26 = test "we can parse the n26 format" $ do
  let line = "\"2019-08-16\",\"Me\",\"DE45100110012622748345\",\"Outgoing Transfer\",\"Money, money\",\"Subscriptions & Donations\",\"-50.0\",\"\",\"\",\"\""
  decodeN26 line $ \result ->
    result === Just (N26Line $ LedgerLine {
         _reference =  "Me"
       , _date = Just (fromGregorian 2019 08 16)
       , _amount = Amount (-50.00)
       , _category = Just "Subscriptions & Donations"
       })

test_parse_revolut = test "we can parse the Revolut format" $ do
  let line = "6 Sep 2019 ; Factory Kitchen ; 5.80 ;  ;  ;  ; 115.80; restaurants;"
  decodeRevolut line $ \result ->
    result === Just (RevolutLine $ LedgerLine {
         _reference = " Factory Kitchen "
       , _date = Just (fromGregorian 2019 09 06)
       , _amount = Amount (-5.80)
       , _category = Just " restaurants"
       })

test_parse_revolut_income = test "we can parse the Revolut format" $ do
  let line = "6 Sep 2019 ; Transfer ; ; 100 ;  ;  ; 33.70; me;"
  decodeRevolut line $ \result ->
    result === Just (RevolutLine $ LedgerLine {
         _reference = " Transfer "
       , _date = Just (fromGregorian 2019 09 06)
       , _amount = Amount 100
       , _category = Just " me"
       })

-- * HELPERS

decodeCommerzbank = decodeLine "Commerzbank" commerzbankHeader
decodeN26         = decodeLine "N26"         n26Header
decodeRevolut     = decodeLine "Revolut"     revolutHeader

decodeLine fileName header line f =
  let
    csvLines =  toS $ unlines [header, line]
    options = makeDecodeOptions fileName
  in
    case decodeByNameWith options csvLines of
      Left e ->
        annotateShow e >> annotateShow csvLines >> failure

      Right (_, vs) ->
        f (head vs)
