module Util
( listsSummingToFour
, pentatopeNumbers
, utf8EncodedFieldData
, NumericScoreWrapper(..)
, TargetList(..)
, DefaultToZeroList(..)
, ScoreTallys(..)
) where

import Types (Target, NumericScore, DefaultToZero(..), IELTSLevel(..), numericScoreRange, targetRange)

import Data.Text.Encoding (encodeUtf8)
import Test.QuickCheck (Arbitrary(..), elements, choose, vectorOf)

import qualified Data.ByteString.Internal as BI
import qualified Data.Text as T

listsSummingToFour :: Int -> [[Int]]
listsSummingToFour n = filter (\xs -> sum xs == 4) $ zeroToFour n
    where
        zeroToFour :: Int -> [[Int]]
        zeroToFour 0 = [[]]
        zeroToFour m = concatMap (\xs -> [xs ++ [a] | a <- [0..4]]) $ zeroToFour (m-1)

pentatopeNumbers :: Int -> [Int]
pentatopeNumbers n = take n $ map (length . listsSummingToFour) [1..]

utf8EncodedFieldData :: Show a => a -> BI.ByteString
utf8EncodedFieldData = encodeUtf8 . T.pack . show

newtype NumericScoreWrapper = NumericScoreWrapper NumericScore
newtype TargetList          = TargetList [Target] deriving Show
newtype DefaultToZeroList   = DefaultToZeroList [DefaultToZero] deriving Show
newtype ScoreTallys         = ScoreTallys (IELTSLevel, [Int]) deriving Show

instance Show NumericScoreWrapper where
    show (NumericScoreWrapper n) = show n

instance Arbitrary NumericScoreWrapper where
    arbitrary = elements $ map NumericScoreWrapper numericScoreRange

instance Arbitrary TargetList where
    arbitrary = do
        l  <- elements $ pentatopeNumbers 5
        xs <- vectorOf l $ elements targetRange
        return $ TargetList xs

instance Arbitrary DefaultToZeroList where
    arbitrary = do
        let l = last $ pentatopeNumbers 5
        xs <- vectorOf l $ elements $ map DefaultToZero [0..4]
        return $ DefaultToZeroList xs

instance Arbitrary ScoreTallys where
    arbitrary = do
        n  <- choose (3, 4)
        xs <- elements $ listsSummingToFour n
        case n of
            3 -> return $ ScoreTallys (L45, xs)
            _ -> do
                l <- elements [L50, L55, L60, L65]
                return $ ScoreTallys (l, xs)
