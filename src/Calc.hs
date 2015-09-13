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
    , lookupIELTSLevel
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

calcTargetIndices :: ScoreGroupMap -> ScoreTallyMap -> Either String [[Int]]
calcTargetIndices msg mst =
    case compare (M.keys msg) (M.keys mst) of
        EQ -> Right $ zipWith zippingFunction (M.toAscList msg) (M.toAscList mst)
        _  -> Left "ScoreGroupMap and ScoreTallyMap keys are not the same (this shouldn't happen)"
    where
        zippingFunction :: (GroupName, ScoreGroup) -> (GroupName, Int) -> [Int]
        zippingFunction (_, sg) (_, st) = V.toList $ V.findIndices (\(DefaultToZero count) -> count == st) cs
            where cs = counts sg

calcTargetIndex :: [[Int]] -> Either String Int
calcTargetIndex xss =
    case f of
        [_] -> Right . head . head $ f
        _   -> Left $ "No unique target index in " ++ show f
    where n = length xss
          f = filter (\xs -> length xs == n) $ group . sort . concat $ xss

calcTarget :: IELTSLevelData -> ListeningScore -> ReadingScore -> WritingScore -> SpeakingScore -> Either String Target
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
                result = case lookupIELTSLevel ielts ieltsLevelDataMap of
                    Left e -> e
                    Right ld -> case calcTarget ld ls rs ws ss of
                        Left  e  -> e
                        Right L1 -> "GM" ++ m ++ show L1
                        Right L2 -> "GM" ++ m ++ show L2
                        Right L3 -> "GM" ++ m ++ show L3
                        Right t  -> show t
