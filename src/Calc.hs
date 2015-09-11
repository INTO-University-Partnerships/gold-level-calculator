module Calc
( calcScoreTallys
, calcTargetIndices
, calcTargetIndex
, calcTarget
, calcManyTargets
) where

import Types
    ( GroupName
    , Target(..)
    , ListeningScore
    , ReadingScore
    , WritingScore
    , SpeakingScore
    , NumericScoreRange(..)
    , LetterScoreRange(..)
    , BoolWrapper(..)
    , NumericScoreWrapper(..)
    , DefaultToZero(..)
    , IELTSLevelData(..)
    , IELTSLevelDataMap
    , ScoreTarget(..)
    , ScoreGroup(..)
    , ScoreGroupMap
    , GOLDCalcParams(..)
    , CSVInput(..)
    , CSVOutput(..)
    )

import Data.List (group, sort)

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

type ScoreTallyMap = M.Map GroupName Int

calcScoreTallys :: ScoreGroupMap -> ListeningScore -> ReadingScore -> WritingScore -> SpeakingScore -> ScoreTallyMap
calcScoreTallys msg ls rs ws ss = M.map getScoreTallyForGroup msg
    where
        getScoreTallyForGroup :: ScoreGroup -> Int
        getScoreTallyForGroup sg = sum $ f [(ls, lsLower, lsUpper), (rs, rsLower, rsUpper)] ++ f [(ws, wsLower, wsUpper), (ss, ssLower, ssUpper)]
            where (NumericScoreRange lsLower lsUpper) = listeningScoreRange sg
                  (NumericScoreRange rsLower rsUpper) = readingScoreRange sg
                  (LetterScoreRange  wsLower wsUpper) = writingScoreRange sg
                  (LetterScoreRange  ssLower ssUpper) = speakingScoreRange sg
                  f :: Ord a => [(a, a, a)] -> [Int]
                  f = map (\(score, lower, upper) -> if score >= lower && score <= upper then 1 else 0)

calcTargetIndices :: ScoreGroupMap -> ScoreTallyMap -> Maybe [[Int]]
calcTargetIndices msg mst =
    case compare (M.keys msg) (M.keys mst) of
        EQ -> Just $ zipWith zippingFunction (M.toAscList msg) (M.toAscList mst)
        _  -> Nothing
    where
        zippingFunction :: (GroupName, ScoreGroup) -> (GroupName, Int) -> [Int]
        zippingFunction (_, sg) (_, st) = V.toList $ V.findIndices (\(DefaultToZero count) -> count == st) cs
            where cs = counts sg

calcTargetIndex :: [[Int]] -> Maybe Int
calcTargetIndex xss =
    case f of
        [_] -> Just . head . head $ f
        _   -> Nothing
    where n = length xss
          f = filter (\xs -> length xs == n) $ group . sort . concat $ xss

calcTarget :: IELTSLevelData -> ListeningScore -> ReadingScore -> WritingScore -> SpeakingScore -> Maybe Target
calcTarget (IELTSLevelData (ScoreTarget _ v) msg) ls rs ws ss = do
    let scoreTallys = calcScoreTallys msg ls rs ws ss
    targetIndices <- calcTargetIndices msg scoreTallys
    targetIndex <- calcTargetIndex targetIndices
    return $ v V.! targetIndex

calcManyTargets :: IELTSLevelDataMap -> V.Vector CSVInput -> V.Vector CSVOutput
calcManyTargets ieltsLevelDataMap vi = V.map f vi
    where
        f :: CSVInput -> CSVOutput
        f i@(CSVInput _ _ _ _ (BoolWrapper pre) (GOLDCalcParams ielts (NumericScoreWrapper ls) (NumericScoreWrapper rs) ws ss)) = CSVOutput i result
            where
                m = if pre then "2" else "1"
                result = case M.lookup ielts ieltsLevelDataMap of
                    Nothing -> "IELTS level " ++ show ielts ++ " not found in IELTS level data map"
                    Just ld -> case calcTarget ld ls rs ws ss of
                        Nothing -> "Something went wrong trying to calculate a score target"
                        Just L1 -> "GM" ++ m ++ show L1
                        Just L2 -> "GM" ++ m ++ show L2
                        Just L3 -> "GM" ++ m ++ show L3
                        Just t  -> show t
