{-# LANGUAGE TemplateHaskell #-}

module TestCalc (testCalc) where

import Util (pentatopeNumbers, ScoreTallys(..), ieltsLevelDataMap)
import Types (IELTSLevel, IELTSLevelData(..), LetterScore, NumericScoreWrapper(..))
import Calc (calcScoreTallys, calcTargetIndices, calcTargetIndex, calcTarget)

import Data.List (nub)
import Data.Maybe (fromJust)
import Test.QuickCheck (Property, (==>))
import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Modifiers (Positive(..), OrderedList(..))

import qualified Data.Map.Strict as M

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
          result = fromJust $ calcTargetIndices msg $ M.fromList $ zip (M.keys msg) xs

prop_calcTargetIndicesHasIndicesInRange :: ScoreTallys -> Bool
prop_calcTargetIndicesHasIndicesInRange (ScoreTallys (l, xs)) = minimum flattenedResult >= 0 && maximum flattenedResult < (pentatopeNumbers 5) !! (M.size msg)
    where (IELTSLevelData _ msg) = fromJust $ M.lookup l ieltsLevelDataMap
          result = fromJust $ calcTargetIndices msg $ M.fromList $ zip (M.keys msg) xs
          flattenedResult = concat result

prop_calcTargetIndicesFail :: ScoreTallys -> Bool
prop_calcTargetIndicesFail (ScoreTallys (l, xs)) =
    case result of
        Just _  -> False
        Nothing -> True
    where (IELTSLevelData _ msg) = fromJust $ M.lookup l ieltsLevelDataMap
          result = calcTargetIndices msg $ M.fromList $ zip ["these", "keys", "are", "wrong"] xs

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

prop_calcTarget :: IELTSLevel -> NumericScoreWrapper -> NumericScoreWrapper -> LetterScore -> LetterScore -> Bool
prop_calcTarget l (NumericScoreWrapper ls) (NumericScoreWrapper rs) ws ss =
    case calcTarget ld ls rs ws ss of
        Nothing -> False
        Just _  -> True
    where ld = fromJust $ M.lookup l ieltsLevelDataMap

return []

testCalc :: IO Bool
testCalc = $quickCheckAll
