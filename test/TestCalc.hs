{-# LANGUAGE TemplateHaskell #-}

module TestCalc (testCalc) where

import Util
    ( pentatopeNumbers
    , fromRight
    , ScoreTallys(..)
    , ieltsLevelDataMap
    , CSVInputList(..)
    , CSVInputListLong(..)
    )

import Calc
    ( calcScoreTallys
    , calcTargetIndices
    , calcTargetIndex
    , calcTarget
    , calcManyTargets
    )

import Types
    ( IELTSLevel
    , IELTSLevelData(..)
    , LetterScore
    , BoolWrapper(..)
    , NumericScoreWrapper(..)
    , CSVInput(..)
    , CSVOutput(..)
    , resultRange
    )

import Data.List (nub, group, sort)
import Data.Maybe (fromJust)
import Test.QuickCheck (Property, (==>))
import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Modifiers (Positive(..), OrderedList(..))

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

prop_calcScoreTallysLengthEqualsScoreGroupLength :: IELTSLevel -> NumericScoreWrapper -> NumericScoreWrapper -> LetterScore -> LetterScore -> Bool
prop_calcScoreTallysLengthEqualsScoreGroupLength l (NumericScoreWrapper ls) (NumericScoreWrapper rs) ws ss = M.size result == M.size msg
    where (IELTSLevelData _ msg) = fromJust $ M.lookup l ieltsLevelDataMap
          result = calcScoreTallys msg ls rs ws ss

prop_calcScoreTallysSumsToFour :: IELTSLevel -> NumericScoreWrapper -> NumericScoreWrapper -> LetterScore -> LetterScore -> Bool
prop_calcScoreTallysSumsToFour l (NumericScoreWrapper ls) (NumericScoreWrapper rs) ws ss = M.foldr (\n acc -> n + acc) 0 result == 4 -- listening, reading, writing, speaking
    where (IELTSLevelData _ msg) = fromJust $ M.lookup l ieltsLevelDataMap
          result = calcScoreTallys msg ls rs ws ss

prop_calcTargetIndicesLengthEqualsScoreGroupLength :: ScoreTallys -> Bool
prop_calcTargetIndicesLengthEqualsScoreGroupLength (ScoreTallys (l, xs)) = length result == M.size msg
    where (IELTSLevelData _ msg) = fromJust $ M.lookup l ieltsLevelDataMap
          result = fromRight $ calcTargetIndices msg $ M.fromList $ zip (M.keys msg) xs

prop_calcTargetIndicesHasIndicesInRange :: ScoreTallys -> Bool
prop_calcTargetIndicesHasIndicesInRange (ScoreTallys (l, xs)) = minimum flattenedResult >= 0 && maximum flattenedResult < (pentatopeNumbers 5) !! (M.size msg)
    where (IELTSLevelData _ msg) = fromJust $ M.lookup l ieltsLevelDataMap
          result = fromRight $ calcTargetIndices msg $ M.fromList $ zip (M.keys msg) xs
          flattenedResult = concat result

prop_calcTargetIndicesFail :: ScoreTallys -> Bool
prop_calcTargetIndicesFail (ScoreTallys (l, xs)) =
    case result of
        Left  e -> e == "ScoreGroupMap and ScoreTallyMap keys are not the same (this shouldn't happen)"
        Right _ -> False
    where (IELTSLevelData _ msg) = fromJust $ M.lookup l ieltsLevelDataMap
          result = calcTargetIndices msg $ M.fromList $ zip ["these", "keys", "are", "wrong"] xs

prop_calcTargetIndexSuccess :: Positive Int -> Positive Int -> Bool
prop_calcTargetIndexSuccess (Positive n) (Positive i) =
    case calcTargetIndex xss of
        Right r -> r == i
        Left  _ -> False
    where xss = replicate n [i]

prop_calcTargetIndexFail :: Positive Int -> OrderedList (Positive Int) -> Property
prop_calcTargetIndexFail (Positive n) (Ordered xs) = length xs' > 1 ==>
    case calcTargetIndex xss of
        Left  e -> e == "No unique target index in " ++ show (group . sort . concat $ xss)
        Right _ -> False
    where xs' = nub $ map getPositive xs
          xss = replicate n xs'

prop_calcTarget :: IELTSLevel -> NumericScoreWrapper -> NumericScoreWrapper -> LetterScore -> LetterScore -> Bool
prop_calcTarget l (NumericScoreWrapper ls) (NumericScoreWrapper rs) ws ss =
    case calcTarget ld ls rs ws ss of
        Left  _ -> False
        Right _ -> True
    where ld = fromJust $ M.lookup l ieltsLevelDataMap

prop_calcManyTargetsOutputSameLengthAsInput :: CSVInputList -> Bool
prop_calcManyTargetsOutputSameLengthAsInput (CSVInputList xs) = V.length result == length xs
    where result = calcManyTargets ieltsLevelDataMap $ V.fromList xs

prop_calcManyTargetsOutputEchosInput :: CSVInputList -> Bool
prop_calcManyTargetsOutputEchosInput (CSVInputList xs) = V.and $ V.zipWith f xs' result
    where xs'    = V.fromList xs
          result = calcManyTargets ieltsLevelDataMap xs'
          f csvInput (CSVOutput csvInput' _) = csvInput == csvInput'

prop_calcManyTargetsLastColumnHasResult :: CSVInputList -> Property
prop_calcManyTargetsLastColumnHasResult (CSVInputList xs) = (not . null $ xs) ==> V.all (\(CSVOutput _ r) -> r `elem` resultRange) result
    where result = calcManyTargets ieltsLevelDataMap $ V.fromList xs

prop_calcManyTargetsSetsModule2IfPreviouslyOnGOLD :: CSVInputListLong -> Bool
prop_calcManyTargetsSetsModule2IfPreviouslyOnGOLD (CSVInputListLong xs) = not (V.null result') && V.all f result'
    where mod1    = filter (\s -> take 3 s == "GM1") resultRange
          mod2    = filter (\s -> take 3 s == "GM2") resultRange
          result  = calcManyTargets ieltsLevelDataMap $ V.fromList xs
          result' = V.filter (\(CSVOutput _ r) -> r `elem` mod1 ++ mod2) result
          f (CSVOutput (CSVInput _ _ _ _ (BoolWrapper p) _) r) = (p && r `elem` mod2) || (not p && r `elem` mod1)

return []

testCalc :: IO Bool
testCalc = $quickCheckAll
