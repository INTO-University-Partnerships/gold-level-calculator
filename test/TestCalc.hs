{-# LANGUAGE TemplateHaskell #-}

module TestCalc (testCalc, pentatopeNumbers) where

import Types
import Calc (calcScoreTallys, calcTargetIndices, calcTargetIndex, calcTarget)
import IOActions (getIELTSLevelDataMap)

import Data.List (nub)
import Data.Maybe (fromJust)
import Test.QuickCheck (Arbitrary(..), elements, choose, vectorOf, Property, (==>))
import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Modifiers (Positive(..), OrderedList(..))
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import qualified Data.Map.Strict as M

newtype NumericScoreWrapper = NumericScoreWrapper NumericScore
newtype TargetList          = TargetList [Target] deriving Show
newtype DefaultToZeroList   = DefaultToZeroList [DefaultToZero] deriving Show
newtype ScoreTallys         = ScoreTallys (IELTSLevel, [Int]) deriving Show

listsSummingToFour :: Int -> [[Int]]
listsSummingToFour n = filter (\xs -> sum xs == 4) $ zeroToFour n
    where
        zeroToFour :: Int -> [[Int]]
        zeroToFour 0 = [[]]
        zeroToFour m = concatMap (\xs -> [xs ++ [a] | a <- [0..4]]) $ zeroToFour (m-1)

pentatopeNumbers :: [Int]
pentatopeNumbers = map (length . listsSummingToFour) [1..5]

instance Show NumericScoreWrapper where
    show (NumericScoreWrapper n) = show n

instance Arbitrary NumericScoreWrapper where
    arbitrary = elements $ map NumericScoreWrapper numericScoreRange

instance Arbitrary TargetList where
    arbitrary = do
        l  <- elements pentatopeNumbers
        xs <- vectorOf l $ elements targetRange
        return $ TargetList xs

instance Arbitrary DefaultToZeroList where
    arbitrary = do
        let l = last pentatopeNumbers
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

prop_calcScoreTallysLengthEqualsScoreGroupLength :: IELTSLevel -> NumericScoreWrapper -> NumericScoreWrapper -> LetterScore -> LetterScore -> Property
prop_calcScoreTallysLengthEqualsScoreGroupLength l (NumericScoreWrapper ls) (NumericScoreWrapper rs) ws ss = monadicIO $ do
    ieltsLevelDataMap <- run $ getIELTSLevelDataMap
    let (IELTSLevelData _ msg) = fromJust $ M.lookup l $ fromJust ieltsLevelDataMap
    let result = calcScoreTallys msg ls rs ws ss
    assert $ M.size result == M.size msg

prop_calcScoreTallysSumsToFour :: IELTSLevel -> NumericScoreWrapper -> NumericScoreWrapper -> LetterScore -> LetterScore -> Property
prop_calcScoreTallysSumsToFour l (NumericScoreWrapper ls) (NumericScoreWrapper rs) ws ss = monadicIO $ do
    ieltsLevelDataMap <- run $ getIELTSLevelDataMap
    let (IELTSLevelData _ msg) = fromJust $ M.lookup l $ fromJust ieltsLevelDataMap
    let result = calcScoreTallys msg ls rs ws ss
    assert $ M.foldr (\n acc -> n + acc) 0 result == 4 -- listening, reading, writing, speaking

prop_calcTargetIndicesLengthEqualsScoreGroupLength :: ScoreTallys -> Property
prop_calcTargetIndicesLengthEqualsScoreGroupLength (ScoreTallys (l, xs)) = monadicIO $ do
    ieltsLevelDataMap <- run $ getIELTSLevelDataMap
    let (IELTSLevelData _ msg) = fromJust $ M.lookup l $ fromJust ieltsLevelDataMap
    let result = fromJust $ calcTargetIndices msg $ M.fromList $ zip (M.keys msg) xs
    assert $ length result == M.size msg

prop_calcTargetIndicesHasIndicesInRange :: ScoreTallys -> Property
prop_calcTargetIndicesHasIndicesInRange (ScoreTallys (l, xs)) = monadicIO $ do
    ieltsLevelDataMap <- run $ getIELTSLevelDataMap
    let (IELTSLevelData _ msg) = fromJust $ M.lookup l $ fromJust ieltsLevelDataMap
    let result = fromJust $ calcTargetIndices msg $ M.fromList $ zip (M.keys msg) xs
    let flattenedResult = concat result
    assert $ minimum flattenedResult >= 0
    assert $ maximum flattenedResult < pentatopeNumbers !! (M.size msg)

prop_calcTargetIndicesFail :: ScoreTallys -> Property
prop_calcTargetIndicesFail (ScoreTallys (l, xs)) = monadicIO $ do
    ieltsLevelDataMap <- run $ getIELTSLevelDataMap
    let (IELTSLevelData _ msg) = fromJust $ M.lookup l $ fromJust ieltsLevelDataMap
    let result = calcTargetIndices msg $ M.fromList $ zip ["these", "keys", "are", "wrong"] xs
    case result of
        Just _  -> assert False
        Nothing -> assert True

prop_calcTargetIndexSuccess :: Positive Int -> Positive Int -> Bool
prop_calcTargetIndexSuccess (Positive n) (Positive i) =
    case calcTargetIndex xss of
        Just r  -> r == i
        Nothing -> False
    where xss = replicate n [i]

prop_calcTargetIndexFail :: Positive Int -> OrderedList (Positive Int) -> Property
prop_calcTargetIndexFail (Positive n) (Ordered xs) = length xs' > 1 ==>
    case calcTargetIndex xss of
        Nothing -> True
        Just _  -> False
    where xs' = nub $ map (\(Positive i) -> i) xs
          xss = replicate n xs'

prop_calcTarget :: IELTSLevel -> NumericScoreWrapper -> NumericScoreWrapper -> LetterScore -> LetterScore -> Property
prop_calcTarget l (NumericScoreWrapper ls) (NumericScoreWrapper rs) ws ss = monadicIO $ do
    ieltsLevelDataMap <- run $ getIELTSLevelDataMap
    let ld = fromJust $ M.lookup l $ fromJust ieltsLevelDataMap
    case calcTarget ld ls rs ws ss of
        Nothing -> assert False
        Just _  -> assert True

return []

testCalc :: IO Bool
testCalc = $quickCheckAll
