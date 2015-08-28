{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Parse
import Types
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Internal as BI
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Csv

import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.All

utf8EncodedFieldData :: Show a => a -> BI.ByteString
utf8EncodedFieldData d = encodeUtf8 $ T.pack $ show d

numericScoreRange :: [Int]
numericScoreRange = [0..100]

letterScoreRange :: [LetterScore]
letterScoreRange = [A1, A1P, A2, A2P, B1, B1P, B2, B2P, C1, C1P, C2, C2P]

instance Arbitrary IELTSLevel where
    arbitrary = elements [L45, L50, L55, L60, L65]

instance Arbitrary LetterScore where
    arbitrary = elements letterScoreRange

instance Arbitrary NumericScoreRange where
    arbitrary = do
        lower <- elements numericScoreRange
        upper <- elements numericScoreRange
        return $ NumericScoreRange lower upper

instance Arbitrary LetterScoreRange where
    arbitrary = do
        lower <- elements letterScoreRange
        upper <- elements letterScoreRange
        return $ LetterScoreRange lower upper

instance Arbitrary Target where
    arbitrary = elements [NoGOLD, L1, L2, L3, Exception, Alert]

newtype TargetList = TargetList [Target] deriving Show
newtype IntList = IntList [Int] deriving Show

instance Arbitrary TargetList where
    arbitrary = do
        xs <- vectorOf (numberOfCsvColumns - targetStartsAtColumn) $ elements [NoGOLD, L1, L2, L3, Exception, Alert]
        return $ TargetList xs

instance Arbitrary IntList where
    arbitrary = do
        xs <- vectorOf (numberOfCsvColumns - targetStartsAtColumn) $ elements numericScoreRange
        return $ IntList xs

prop_parseField_IELTSLevel_success :: IELTSLevel -> Bool
prop_parseField_IELTSLevel_success l =
    case runParser (parseField (utf8EncodedFieldData l) :: Parser IELTSLevel) of
        Right l' -> l == l'
        Left  _  -> False

prop_parseField_IELTSLevel_fail :: String -> Property
prop_parseField_IELTSLevel_fail l = l /= show l ==>
    case runParser (parseField (utf8EncodedFieldData l) :: Parser IELTSLevel) of
        Right _ -> False
        Left  _ -> True

prop_parseField_NumericScoreRange_success :: Positive Int -> Positive Int -> Property
prop_parseField_NumericScoreRange_success (Positive lower) (Positive upper) = lower <= 100 && upper <= 100 ==>
    case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser NumericScoreRange) of
        Right (NumericScoreRange lower' upper') -> lower' == lower && upper' == upper
        Left _ -> False
    where f = show lower ++ " to " ++ show upper

prop_parseField_NumericScoreRange_fail :: String -> Bool
prop_parseField_NumericScoreRange_fail f =
    case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser NumericScoreRange) of
        Right _ -> False
        Left  _ -> True

prop_parseField_LetterScoreRange_success :: LetterScore -> LetterScore -> Bool
prop_parseField_LetterScoreRange_success lower upper =
    case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser LetterScoreRange) of
        Right (LetterScoreRange lower' upper') -> lower' == lower && upper' == upper
        Left _ -> False
    where f = show lower ++ " to " ++ show upper

prop_parseField_LetterScoreRange_fail :: String -> Bool
prop_parseField_LetterScoreRange_fail f =
    case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser LetterScoreRange) of
        Right _ -> False
        Left  _ -> True

prop_parseField_Target_success :: Target -> Bool
prop_parseField_Target_success t =
    case runParser (parseField (utf8EncodedFieldData t) :: Parser Target) of
        Right t' -> t == t'
        Left  _  -> False

prop_parseField_Target_fail :: String -> Property
prop_parseField_Target_fail t = t /= show t ==>
    case runParser (parseField (utf8EncodedFieldData t) :: Parser Target) of
        Right _ -> False
        Left  _ -> True

prop_parseRecord_ScoreTarget_success :: IELTSLevel -> TargetList -> Bool
prop_parseRecord_ScoreTarget_success l (TargetList ts) =
    case runParser (parseRecord r :: Parser ScoreTarget) of
        Right (ScoreTarget l' v) -> l == l' && V.toList v == ts
        Left _ -> False
    where r = V.fromList $ map (encodeUtf8 . T.pack) $ [show l] ++ (replicate 6 "") ++ map show ts

prop_parseRecord_ScoreTarget_fail :: [String] -> Bool
prop_parseRecord_ScoreTarget_fail xs =
    case runParser (parseRecord r :: Parser ScoreTarget) of
        Right _ -> False
        Left  _ -> True
    where r = V.fromList $ map (encodeUtf8 . T.pack) $ xs

prop_parseRecord_ScoreGroup_success :: IELTSLevel -> String -> NumericScoreRange -> NumericScoreRange -> LetterScoreRange -> LetterScoreRange -> IntList -> Property
prop_parseRecord_ScoreGroup_success l n ls rs ws ss (IntList ms) = (not . null) n ==>
    case runParser (parseRecord r :: Parser ScoreGroup) of
        Right (ScoreGroup l' n' ls' rs' ws' ss' v) -> l' == l && n' == n && ls' == ls && rs' == rs && ws' == ws && ss' == ss && V.toList v == ms
        Left _ -> False
    where r = V.fromList $ map (encodeUtf8 . T.pack) $ [show l, n, show ls, show rs, show ws, show ss, ""] ++ map show ms

prop_parseRecord_ScoreGroup_fail :: [String] -> Bool
prop_parseRecord_ScoreGroup_fail xs =
    case runParser (parseRecord r :: Parser ScoreGroup) of
        Right _ -> False
        Left  _ -> True
    where r = V.fromList $ map (encodeUtf8 . T.pack) $ xs

return []
main = $quickCheckAll
