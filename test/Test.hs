{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Parse
import Types
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Internal as BI
import qualified Data.Text as T
import Data.Csv

import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.All

utf8EncodedFieldData :: Show a => a -> BI.ByteString
utf8EncodedFieldData d = encodeUtf8 $ T.pack $ show d

instance Arbitrary IELTSLevel where
    arbitrary = elements [L45, L50, L55, L60, L65]

instance Arbitrary LetterScore where
    arbitrary = elements [A1, A1P, A2, A2P, B1, B1P, B2, B2P, C1, C1P, C2, C2P]

instance Arbitrary Target where
    arbitrary = elements [NoGOLD, L1, L2, L3, Exception, Alert]

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
        Left  _ -> False
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
        Left  _ -> False
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

return []
main = $quickCheckAll
