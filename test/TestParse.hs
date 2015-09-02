{-# LANGUAGE TemplateHaskell #-}

module TestParse (
    testParse
) where

import Types

import Data.Csv (Parser, parseField, parseRecord, runParser)
import Data.Text.Encoding (encodeUtf8)
import Test.QuickCheck (Arbitrary(..), elements, vectorOf, Property, (==>))
import Test.QuickCheck.All (quickCheckAll)

import qualified Data.ByteString.Internal as BI
import qualified Data.Text as T
import qualified Data.Vector as V

utf8EncodedFieldData :: Show a => a -> BI.ByteString
utf8EncodedFieldData = encodeUtf8 . T.pack . show

magicConstants :: [Int]
magicConstants = [1, 5, 15, 34, 65]

newtype NumericScoreWrapper = NumericScoreWrapper NumericScore
newtype TargetList          = TargetList [Target] deriving Show
newtype DefaultToZeroList   = DefaultToZeroList [DefaultToZero] deriving Show

instance Show NumericScoreWrapper where
    show (NumericScoreWrapper n) = show n

instance Arbitrary NumericScoreWrapper where
    arbitrary = elements $ map NumericScoreWrapper numericScoreRange

instance Arbitrary TargetList where
    arbitrary = do
        l  <- elements magicConstants
        xs <- vectorOf l $ elements targetRange
        return $ TargetList xs

instance Arbitrary DefaultToZeroList where
    arbitrary = do
        let l = last magicConstants
        xs <- vectorOf l $ elements $ map DefaultToZero [0..4]
        return $ DefaultToZeroList xs

prop_parseFieldIELTSLevelSuccess :: IELTSLevel -> Bool
prop_parseFieldIELTSLevelSuccess l =
    case runParser (parseField (utf8EncodedFieldData l) :: Parser IELTSLevel) of
        Right l' -> l == l'
        Left  _  -> False

prop_parseFieldIELTSLevelFail :: String -> Property
prop_parseFieldIELTSLevelFail l = l /= show l ==>
    case runParser (parseField (utf8EncodedFieldData l) :: Parser IELTSLevel) of
        Right _ -> False
        Left  _ -> True

prop_parseFieldNumericScoreRangeSuccess :: NumericScoreRange -> Bool
prop_parseFieldNumericScoreRangeSuccess nsr@(NumericScoreRange lower upper) =
    case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser NumericScoreRange) of
        Right (NumericScoreRange lower' upper') -> lower' == lower && upper' == upper
        Left _ -> False
    where f = show nsr

prop_parseFieldNumericScoreRangeLowerGTUpperFail :: NumericScoreRange -> Property
prop_parseFieldNumericScoreRangeLowerGTUpperFail (NumericScoreRange lower upper) = upper > lower ==>
    case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser NumericScoreRange) of
        Right _ -> False
        Left  _ -> True
    where f = show $ NumericScoreRange upper lower

prop_parseFieldNumericScoreRangeFail :: String -> Bool
prop_parseFieldNumericScoreRangeFail f =
    case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser NumericScoreRange) of
        Right _ -> False
        Left  _ -> True

prop_parseFieldLetterScoreRangeSuccess :: LetterScoreRange -> Bool
prop_parseFieldLetterScoreRangeSuccess lsr@(LetterScoreRange lower upper) =
    case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser LetterScoreRange) of
        Right (LetterScoreRange lower' upper') -> lower' == lower && upper' == upper
        Left _ -> False
    where f = show lsr

prop_parseFieldLetterScoreRangeLowerGTUpperFail :: LetterScoreRange -> Property
prop_parseFieldLetterScoreRangeLowerGTUpperFail (LetterScoreRange lower upper) = upper > lower ==>
    case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser LetterScoreRange) of
        Right _ -> False
        Left  _ -> True
    where f = show $ LetterScoreRange upper lower

prop_parseFieldLetterScoreRangeFail :: String -> Bool
prop_parseFieldLetterScoreRangeFail f =
    case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser LetterScoreRange) of
        Right _ -> False
        Left  _ -> True

prop_parseFieldTargetSuccess :: Target -> Bool
prop_parseFieldTargetSuccess t =
    case runParser (parseField (utf8EncodedFieldData t) :: Parser Target) of
        Right t' -> t == t'
        Left  _  -> False

prop_parseFieldTargetFail :: String -> Property
prop_parseFieldTargetFail t = t /= show t ==>
    case runParser (parseField (utf8EncodedFieldData t) :: Parser Target) of
        Right _ -> False
        Left  _ -> True

prop_parseRecordScoreTargetSuccess :: IELTSLevel -> TargetList -> Bool
prop_parseRecordScoreTargetSuccess l (TargetList ts) =
    case runParser (parseRecord r :: Parser ScoreTarget) of
        Right (ScoreTarget l' v) -> l == l' && V.toList v == ts
        Left _ -> False
    where r = V.fromList $ map (encodeUtf8 . T.pack) $ [show l] ++ (replicate 6 "") ++ map show ts

prop_parseRecordScoreTargetFail :: [String] -> Bool
prop_parseRecordScoreTargetFail xs =
    case runParser (parseRecord r :: Parser ScoreTarget) of
        Right _ -> False
        Left  _ -> True
    where r = V.fromList $ map (encodeUtf8 . T.pack) $ xs

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
    where r = V.fromList $ map (encodeUtf8 . T.pack) $ [show l, n, show ls, show rs, show ws, show ss, ""] ++ map show cs

prop_parseRecordScoreGroupFail :: [String] -> Bool
prop_parseRecordScoreGroupFail xs =
    case runParser (parseRecord r :: Parser ScoreGroup) of
        Right _ -> False
        Left  _ -> True
    where r = V.fromList $ map (encodeUtf8 . T.pack) $ xs

return []

testParse :: IO Bool
testParse = $quickCheckAll
