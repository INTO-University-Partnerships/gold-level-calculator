{-# LANGUAGE TemplateHaskell #-}

module TestCalc (
    testCalc
) where

import Types
import Calc (calcScoreTallys)
import IOActions (getIELTSLevelDataMap)

import Data.Maybe (fromJust)
import Test.QuickCheck (Arbitrary(..), elements, vectorOf, Property)
import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import qualified Data.Map.Strict as M

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

prop_calcScoreTallysLengthEqualsScoreGroupLength :: IELTSLevel -> NumericScoreWrapper -> NumericScoreWrapper -> LetterScore -> LetterScore -> Property
prop_calcScoreTallysLengthEqualsScoreGroupLength l (NumericScoreWrapper ls) (NumericScoreWrapper rs) ws ss = monadicIO $ do
    ieltsLevelDataMap <- run $ getIELTSLevelDataMap
    let ieltsLevelData = fromJust $ M.lookup l $ fromJust ieltsLevelDataMap
    let result = calcScoreTallys ieltsLevelData ls rs ws ss
    assert $ M.size result == M.size (scoreGroups ieltsLevelData)

prop_calcScoreTallysSumsToFour :: IELTSLevel -> NumericScoreWrapper -> NumericScoreWrapper -> LetterScore -> LetterScore -> Property
prop_calcScoreTallysSumsToFour l (NumericScoreWrapper ls) (NumericScoreWrapper rs) ws ss = monadicIO $ do
    ieltsLevelDataMap <- run $ getIELTSLevelDataMap
    let ieltsLevelData = fromJust $ M.lookup l $ fromJust ieltsLevelDataMap
    let result = calcScoreTallys ieltsLevelData ls rs ws ss
    assert $ M.foldr (\n acc -> n + acc) 0 result == 4 -- listening, reading, writing, speaking

return []

testCalc :: IO Bool
testCalc = $quickCheckAll
