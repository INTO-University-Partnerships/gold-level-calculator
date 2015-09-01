{-# LANGUAGE TemplateHaskell #-}

import Types
import Parse
import Calc

import Data.Csv (Parser, parseField, parseRecord, runParser)
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromJust)

import qualified Data.ByteString.Internal as BI
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

import Test.QuickCheck
import Test.QuickCheck.All

{--}

utf8EncodedFieldData :: Show a => a -> BI.ByteString
utf8EncodedFieldData = encodeUtf8 . T.pack . show

ieltsRange :: [IELTSLevel]
ieltsRange = [L45, L50, L55, L60, L65]

numericScoreRange :: [Int]
numericScoreRange = [0..100]

letterScoreRange :: [LetterScore]
letterScoreRange = [A1, A1P, A2, A2P, B1, B1P, B2, B2P, C1, C1P, C2]

targetRange :: [Target]
targetRange = [NoGOLD, L1, L2, L3, Exception, Alert, Blank]

magicConstants :: [Int]
magicConstants = [1, 5, 15, 34, 65]

scoreGroupsL65 :: ScoreGroupMap
scoreGroupsL65 = M.fromList [
    ("Over",      ScoreGroup L65 "Over"      (NumericScoreRange  76 100) (NumericScoreRange  76 100) (LetterScoreRange  C1  C2) (LetterScoreRange  C1  C2) V.empty),
    ("Threshold", ScoreGroup L65 "Threshold" (NumericScoreRange  67  75) (NumericScoreRange  67  75) (LetterScoreRange B2P B2P) (LetterScoreRange B2P B2P) V.empty),
    ("Middle",    ScoreGroup L65 "Middle"    (NumericScoreRange  60  66) (NumericScoreRange  60  66) (LetterScoreRange  B2  B2) (LetterScoreRange  B2  B2) V.empty),
    ("Deep",      ScoreGroup L65 "Deep"      (NumericScoreRange   0  59) (NumericScoreRange   0  59) (LetterScoreRange  A1 B1P) (LetterScoreRange  A1 B1P) V.empty)
    ]

{--}

newtype NumericScoreWrapper = NumericScoreWrapper NumericScore
newtype TargetList          = TargetList [Target] deriving Show
newtype DefaultToZeroList   = DefaultToZeroList [DefaultToZero] deriving Show

{--}

instance Show NumericScoreWrapper where
    show (NumericScoreWrapper n) = show n

instance Arbitrary IELTSLevel where
    arbitrary = elements ieltsRange

instance Arbitrary NumericScoreWrapper where
    arbitrary = elements $ map NumericScoreWrapper numericScoreRange

instance Arbitrary LetterScore where
    arbitrary = elements letterScoreRange

instance Arbitrary NumericScoreRange where
    arbitrary = do
        s1 <- elements numericScoreRange
        s2 <- elements numericScoreRange
        return $ NumericScoreRange (min s1 s2) (max s1 s2)

instance Arbitrary LetterScoreRange where
    arbitrary = do
        s1 <- elements letterScoreRange
        s2 <- elements letterScoreRange
        return $ LetterScoreRange (min s1 s2) (max s1 s2)

instance Arbitrary Target where
    arbitrary = elements targetRange

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

{--}

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
prop_parseFieldNumericScoreRangeLowerGTUpperFail nsr@(NumericScoreRange lower upper) = upper > lower ==>
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
prop_parseFieldLetterScoreRangeLowerGTUpperFail lsr@(LetterScoreRange lower upper) = upper > lower ==>
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

prop_calcScoreTallysReturnsNothingIfIELTSLevelNotFound :: IELTSLevel -> NumericScoreWrapper -> NumericScoreWrapper -> LetterScore -> LetterScore -> Bool
prop_calcScoreTallysReturnsNothingIfIELTSLevelNotFound l (NumericScoreWrapper ls) (NumericScoreWrapper rs) ws ss =
    calcScoreTallys M.empty l ls rs ws ss == Nothing

prop_calcScoreTallysLengthEqualsScoreGroupLength :: NumericScoreWrapper -> NumericScoreWrapper -> LetterScore -> LetterScore -> Bool
prop_calcScoreTallysLengthEqualsScoreGroupLength (NumericScoreWrapper ls) (NumericScoreWrapper rs) ws ss =
    M.size (fromJust result) == M.size scoreGroupsL65
    where ieltsLevelData    = IELTSLevelData (ScoreTarget L65 V.empty) scoreGroupsL65
          ieltsLevelDataMap = M.insert L65 ieltsLevelData M.empty
          result            = calcScoreTallys ieltsLevelDataMap L65 ls rs ws ss

prop_calcScoreTallysSumsToScoreGroupLength :: NumericScoreWrapper -> NumericScoreWrapper -> LetterScore -> LetterScore -> Bool
prop_calcScoreTallysSumsToScoreGroupLength (NumericScoreWrapper ls) (NumericScoreWrapper rs) ws ss =
    M.foldr (\n acc -> n + acc) 0 (fromJust result) == M.size scoreGroupsL65
    where ieltsLevelData    = IELTSLevelData (ScoreTarget L65 V.empty) scoreGroupsL65
          ieltsLevelDataMap = M.insert L65 ieltsLevelData M.empty
          result            = calcScoreTallys ieltsLevelDataMap L65 ls rs ws ss

{--}

return []
main = $quickCheckAll
