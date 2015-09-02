{-# LANGUAGE OverloadedStrings #-}

module Calc (
    calcScoreTallys,
    calcTargetIndices
) where

import Types (
    GroupName,
    ListeningScore,
    ReadingScore,
    WritingScore,
    SpeakingScore,
    NumericScoreRange(..),
    LetterScoreRange(..),
    ScoreGroup(..),
    IELTSLevelData(..)
    )

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

calcScoreTallys :: IELTSLevelData -> ListeningScore -> ReadingScore -> WritingScore -> SpeakingScore -> M.Map GroupName Int
calcScoreTallys (IELTSLevelData _ msg) ls rs ws ss = M.map getScoreTallyForGroup msg
    where
        getScoreTallyForGroup :: ScoreGroup -> Int
        getScoreTallyForGroup sg = sum $ f [(ls, lsLower, lsUpper), (rs, rsLower, rsUpper)] ++ f [(ws, wsLower, wsUpper), (ss, ssLower, ssUpper)]
            where (NumericScoreRange lsLower lsUpper) = listeningScoreRange sg
                  (NumericScoreRange rsLower rsUpper) = readingScoreRange sg
                  (LetterScoreRange  wsLower wsUpper) = writingScoreRange sg
                  (LetterScoreRange  ssLower ssUpper) = speakingScoreRange sg
                  f :: Ord a => [(a, a, a)] -> [Int]
                  f = map (\(score, lower, upper) -> if score >= lower && score <= upper then 1 else 0)

calcTargetIndices :: IELTSLevelData -> M.Map GroupName Int -> M.Map GroupName (V.Vector Int)
calcTargetIndices = undefined
