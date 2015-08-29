{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Types
import Parse
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Internal as BI
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Csv

import Test.QuickCheck
import Test.QuickCheck.All

utf8EncodedFieldData :: Show a => a -> BI.ByteString
utf8EncodedFieldData = encodeUtf8 . T.pack . show

ieltsRange :: [IELTSLevel]
ieltsRange = [L45, L50, L55, L60, L65]

numericScoreRange :: [Int]
numericScoreRange = [0..100]

letterScoreRange :: [LetterScore]
letterScoreRange = [A1, A1P, A2, A2P, B1, B1P, B2, B2P, C1, C1P, C2]

targetRange :: [Target]
targetRange = [NoGOLD, L1, L2, L3, Exception, Alert]

magicConstants :: [Int]
magicConstants = [1, 5, 15, 34, 65]

instance Arbitrary IELTSLevel where
    arbitrary = elements ieltsRange

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

newtype TargetList = TargetList [Target] deriving Show
newtype DefaultToZeroList = DefaultToZeroList [DefaultToZero] deriving Show

instance Arbitrary TargetList where
    arbitrary = do
        l  <- elements magicConstants
        xs <- vectorOf l $ elements targetRange
        return $ TargetList xs

instance Arbitrary DefaultToZeroList where
    arbitrary = do
        l  <- elements magicConstants
        xs <- vectorOf (l-1) $ elements $ map DefaultToZero [0..4]
        y  <- elements $ map DefaultToZero [1..4]
        return $ DefaultToZeroList $ xs ++ [y]

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

prop_parseField_NumericScoreRange_success :: NumericScoreRange -> Bool
prop_parseField_NumericScoreRange_success nsr@(NumericScoreRange lower upper) =
    case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser NumericScoreRange) of
        Right (NumericScoreRange lower' upper') -> lower' == lower && upper' == upper
        Left _ -> False
    where f = show nsr

prop_parseField_NumericScoreRange_lower_gt_upper_fail :: NumericScoreRange -> Property
prop_parseField_NumericScoreRange_lower_gt_upper_fail nsr@(NumericScoreRange lower upper) = upper > lower ==>
    case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser NumericScoreRange) of
        Right _ -> False
        Left  _ -> True
    where f = show $ NumericScoreRange upper lower

prop_parseField_NumericScoreRange_fail :: String -> Bool
prop_parseField_NumericScoreRange_fail f =
    case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser NumericScoreRange) of
        Right _ -> False
        Left  _ -> True

prop_parseField_LetterScoreRange_success :: LetterScoreRange -> Bool
prop_parseField_LetterScoreRange_success lsr@(LetterScoreRange lower upper) =
    case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser LetterScoreRange) of
        Right (LetterScoreRange lower' upper') -> lower' == lower && upper' == upper
        Left _ -> False
    where f = show lsr

prop_parseField_LetterScoreRange_lower_gt_upper_fail :: LetterScoreRange -> Property
prop_parseField_LetterScoreRange_lower_gt_upper_fail lsr@(LetterScoreRange lower upper) = upper > lower ==>
    case runParser (parseField (encodeUtf8 $ T.pack f) :: Parser LetterScoreRange) of
        Right _ -> False
        Left  _ -> True
    where f = show $ LetterScoreRange upper lower

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

prop_parseRecord_ScoreGroup_success :: IELTSLevel -> String -> NumericScoreRange -> NumericScoreRange -> LetterScoreRange -> LetterScoreRange -> DefaultToZeroList -> Property
prop_parseRecord_ScoreGroup_success l n ls rs ws ss (DefaultToZeroList cs) = (not . null) n ==>
    case runParser (parseRecord r :: Parser ScoreGroup) of
        Right (ScoreGroup l' n' ls' rs' ws' ss' v) -> l' == l && n' == n && ls' == ls && rs' == rs && ws' == ws && ss' == ss && V.toList v == cs
        Left _ -> False
    where r = V.fromList $ map (encodeUtf8 . T.pack) $ [show l, n, show ls, show rs, show ws, show ss, ""] ++ map show cs

prop_parseRecord_ScoreGroup_fail :: [String] -> Bool
prop_parseRecord_ScoreGroup_fail xs =
    case runParser (parseRecord r :: Parser ScoreGroup) of
        Right _ -> False
        Left  _ -> True
    where r = V.fromList $ map (encodeUtf8 . T.pack) $ xs

return []
main = $quickCheckAll
