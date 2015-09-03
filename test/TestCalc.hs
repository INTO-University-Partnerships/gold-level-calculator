{-# LANGUAGE TemplateHaskell #-}

module TestCalc (testCalc) where

import Util (pentatopeNumbers, NumericScoreWrapper(..), ScoreTallys(..))
import Types (IELTSLevel, IELTSLevelData(..), LetterScore)
import Calc (calcScoreTallys, calcTargetIndices, calcTargetIndex, calcTarget)
import IOActions (getIELTSLevelDataMap)

import Data.List (nub)
import Data.Maybe (fromJust)
import Test.QuickCheck (Property, (==>))
import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Modifiers (Positive(..), OrderedList(..))
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import qualified Data.Map.Strict as M

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
