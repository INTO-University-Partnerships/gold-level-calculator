{-# LANGUAGE TemplateHaskell #-}

module TestTypes (testTypes) where

import Util (utf8EncodedFieldData, TargetList(..), DefaultToZeroList(..))

import Types
  ( IELTSLevel
  , NumericScoreRange(..)
  , LetterScoreRange(..)
  , Target
  , ScoreTarget(..)
  , ScoreGroup(..)
  , CSVInput(..)
  , CSVOutput(..)
  , GOLDCalcParams(..)
  , BoolWrapper
  , NumericScoreWrapper(..)
  , LetterScore(..)
  , enc
  , ieltsRange
  , targetRange
  , letterScoreRange
  )

import Data.Csv (Parser, parseField, parseRecord, runParser, toRecord)
import Data.List (intersperse, isInfixOf)
import Data.Text.Encoding (encodeUtf8)
import Test.QuickCheck (Property, (==>))
import Test.QuickCheck.All (quickCheckAll)

import qualified Data.Text as T
import qualified Data.Vector as V

prop_parseFieldBoolWrapperSuccess :: BoolWrapper -> Bool
prop_parseFieldBoolWrapperSuccess b =
  case runParser (parseField (utf8EncodedFieldData b) :: Parser BoolWrapper) of
    Right b' -> b == b'
    Left  _  -> False

prop_parseFieldBoolWrapperFail :: Char -> Property
prop_parseFieldBoolWrapperFail f = (not $ elem f ['Y', 'y', 'N', 'n']) ==>
  case runParser (parseField (encodeUtf8 $ T.pack [f]) :: Parser BoolWrapper) of
    Right _ -> False
    Left  e -> msg `isInfixOf` e
  where
  msg = "\"" ++ [f] ++ "\" is not one of ['Y', 'N']"

prop_parseFieldNumericScoreWrapperSuccess :: NumericScoreWrapper -> Bool
prop_parseFieldNumericScoreWrapperSuccess (NumericScoreWrapper n) =
  case runParser (parseField (utf8EncodedFieldData n) :: Parser NumericScoreWrapper) of
    Right (NumericScoreWrapper n') -> n == n'
    Left  _  -> False

prop_parseFieldNumericScoreWrapperFail :: Int -> Property
prop_parseFieldNumericScoreWrapperFail n = n < 0 || n > 100 ==>
  case runParser (parseField (utf8EncodedFieldData n) :: Parser NumericScoreWrapper) of
    Right _ -> False
    Left  e -> msg `isInfixOf` e
  where
  msg = "\"" ++ show n ++ "\" is not an integer in the range [0..100] inclusive"

prop_parseFieldIELTSLevelSuccess :: IELTSLevel -> Bool
prop_parseFieldIELTSLevelSuccess l =
  case runParser (parseField (utf8EncodedFieldData l) :: Parser IELTSLevel) of
    Right l' -> l == l'
    Left  _  -> False

prop_parseFieldIELTSLevelFail :: String -> Property
prop_parseFieldIELTSLevelFail l = (not $ elem l $ map show ieltsRange) ==>
  case runParser (parseField (encodeUtf8 . T.pack $ l) :: Parser IELTSLevel) of
    Right _ -> False
    Left  e -> msg `isInfixOf` e
  where
  msg = "\"" ++ l ++ "\" is not one of [" ++ (concat $ intersperse ", " $ map show ieltsRange) ++ "]"

prop_parseFieldNumericScoreRangeSuccess :: NumericScoreRange -> Bool
prop_parseFieldNumericScoreRangeSuccess nsr@(NumericScoreRange lower upper) =
  case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser NumericScoreRange) of
    Right (NumericScoreRange lower' upper') -> lower' == lower && upper' == upper
    Left _ -> False
  where
  f = show nsr

prop_parseFieldNumericScoreRangeLowerGTUpperFail :: NumericScoreRange -> Property
prop_parseFieldNumericScoreRangeLowerGTUpperFail (NumericScoreRange lower upper) = upper > lower ==>
  case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser NumericScoreRange) of
    Right _ -> False
    Left  e -> msg `isInfixOf` e
  where
  f   = show $ NumericScoreRange upper lower
  msg = "\"" ++ f ++ "\" is not a valid numeric score range"

prop_parseFieldNumericScoreRangeFail :: String -> Bool
prop_parseFieldNumericScoreRangeFail f =
  case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser NumericScoreRange) of
    Right _ -> False
    Left  e -> msg `isInfixOf` e
  where
  msg = "\"" ++ f ++ "\" is not a valid numeric score range"

prop_parseFieldLetterScoreSuccess :: LetterScore -> Bool
prop_parseFieldLetterScoreSuccess ls =
  case runParser (parseField (utf8EncodedFieldData ls) :: Parser LetterScore) of
    Right ls' -> ls' == ls
    Left  _   -> False

prop_parseFieldLetterScoreFail :: String -> Property
prop_parseFieldLetterScoreFail f = (not $ elem f $ map show letterScoreRange) ==>
  case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser LetterScore) of
    Right _ -> False
    Left  e -> msg `isInfixOf` e
  where
  msg = "\"" ++ f ++ "\" is not one of [" ++ (concat $ intersperse ", " $ map show letterScoreRange) ++ "]"

prop_parseFieldLetterScoreRangeSuccess :: LetterScoreRange -> Bool
prop_parseFieldLetterScoreRangeSuccess lsr@(LetterScoreRange lower upper) =
  case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser LetterScoreRange) of
    Right (LetterScoreRange lower' upper') -> lower' == lower && upper' == upper
    Left _ -> False
  where
  f = show lsr

prop_parseFieldLetterScoreRangeLowerGTUpperFail :: LetterScoreRange -> Property
prop_parseFieldLetterScoreRangeLowerGTUpperFail (LetterScoreRange lower upper) = upper > lower ==>
  case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser LetterScoreRange) of
    Right _ -> False
    Left  e -> msg `isInfixOf` e
  where
  f   = show $ LetterScoreRange upper lower
  msg = "\"" ++ f ++ "\" is not a valid letter score range"

prop_parseFieldLetterScoreRangeFail :: String -> Bool
prop_parseFieldLetterScoreRangeFail f =
  case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser LetterScoreRange) of
    Right _ -> False
    Left  e -> msg `isInfixOf` e
  where
  msg = "\"" ++ f ++ "\" is not a valid letter score range"

prop_parseFieldTargetSuccess :: Target -> Bool
prop_parseFieldTargetSuccess t =
  case runParser (parseField (utf8EncodedFieldData t) :: Parser Target) of
    Right t' -> t == t'
    Left  _  -> False

prop_parseFieldTargetFail :: String -> Property
prop_parseFieldTargetFail t = (not . null $ t) && (not $ elem t $ map show $ init targetRange) ==>
  case runParser (parseField (encodeUtf8 . T.pack $ t) :: Parser Target) of
    Right _ -> False
    Left  e -> msg `isInfixOf` e
  where
  msg = "\"" ++ t ++ "\" is not one of [" ++ (concat $ intersperse ", " $ map show $ init targetRange) ++ "]"

prop_parseRecordScoreTargetSuccess :: IELTSLevel -> TargetList -> Bool
prop_parseRecordScoreTargetSuccess l (TargetList ts) =
  case runParser (parseRecord r :: Parser ScoreTarget) of
    Right (ScoreTarget l' v) -> l == l' && V.toList v == ts
    Left _ -> False
  where
  r = V.fromList $ map (encodeUtf8 . T.pack) $ [show l] ++ (replicate 6 "") ++ map show ts

prop_parseRecordScoreTargetFail :: [String] -> Bool
prop_parseRecordScoreTargetFail xs =
  case runParser (parseRecord r :: Parser ScoreTarget) of
    Right _ -> False
    Left  _ -> True
  where
  r = V.fromList $ map (encodeUtf8 . T.pack) $ xs

prop_parseRecordScoreGroupSuccess :: IELTSLevel
                   -> String
                   -> NumericScoreRange
                   -> NumericScoreRange
                   -> LetterScoreRange
                   -> LetterScoreRange
                   -> DefaultToZeroList
                   -> Property
prop_parseRecordScoreGroupSuccess l n ls rs ws ss (DefaultToZeroList cs) = (not . null) n ==>
  case runParser (parseRecord r :: Parser ScoreGroup) of
    Right (ScoreGroup l' n' ls' rs' ws' ss' v) -> l' == l && n' == n && ls' == ls && rs' == rs && ws' == ws && ss' == ss && V.toList v == cs
    Left _ -> False
  where
  r = V.fromList $ map (encodeUtf8 . T.pack) $ [show l, n, show ls, show rs, show ws, show ss, ""] ++ map show cs

prop_parseRecordScoreGroupFail :: [String] -> Bool
prop_parseRecordScoreGroupFail xs =
  case runParser (parseRecord r :: Parser ScoreGroup) of
    Right _ -> False
    Left  _ -> True
  where
  r = V.fromList $ map (encodeUtf8 . T.pack) $ xs

prop_parseRecordCSVInputSuccess :: CSVInput -> Bool
prop_parseRecordCSVInputSuccess i@(CSVInput s l f c p (GOLDCalcParams ielts ls rs ws ss)) =
  case runParser (parseRecord r :: Parser CSVInput) of
    Right i' -> i' == i
    Left  _  -> False
  where
  r = V.fromList $ map (encodeUtf8 . T.pack) $ [s, l, f, c] ++ [show p, show ielts] ++ map show [ls, rs] ++ map show [ws, ss]

prop_parseRecordCSVInputFail :: [String] -> Bool
prop_parseRecordCSVInputFail xs =
  case runParser (parseRecord r :: Parser CSVInput) of
    Right _ -> False
    Left  _ -> True
  where
  r = V.fromList $ map (encodeUtf8 . T.pack) $ xs

prop_toRecordCSVInput :: CSVInput -> Bool
prop_toRecordCSVInput i =
  case runParser (parseRecord r :: Parser CSVInput) of
    Right i' -> i' == i
    Left  _  -> False
  where
  r = toRecord i

prop_toRecordCSVOutput :: CSVOutput -> Bool
prop_toRecordCSVOutput o@(CSVOutput i r) = o' == V.snoc i' (enc r)
  where
  o' = toRecord o
  i' = toRecord i

return []

testTypes :: IO Bool
testTypes = $quickCheckAll
