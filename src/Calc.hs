{-# LANGUAGE OverloadedStrings #-}

module Calc (
    calcScoreTallys
) where

import Types (
    IELTSLevel,
    GroupName,
    ListeningScore,
    ReadingScore,
    WritingScore,
    SpeakingScore,
    NumericScoreRange(..),
    LetterScoreRange(..),
    ScoreGroup(..),
    Target(..),
    IELTSLevelData(..),
    IELTSLevelDataMap
    )

import qualified Data.Map.Strict as M

calcScoreTallys :: IELTSLevelDataMap
                 -> IELTSLevel
                 -> ListeningScore
                 -> ReadingScore
                 -> WritingScore
                 -> SpeakingScore
                 -> Maybe (M.Map GroupName Int)
calcScoreTallys m l ls rs ws ss = do
    case ieltsLevelData of
        Nothing -> Nothing
        Just (IELTSLevelData st msg) -> Just $ M.map getScoreTallyForGroup msg
    where
        ieltsLevelData = M.lookup l m
        getScoreTallyForGroup :: ScoreGroup -> Int
        getScoreTallyForGroup sg = sum $ f [(ls, lsLower, lsUpper), (rs, rsLower, rsUpper)] ++ f [(ws, wsLower, wsUpper), (ss, ssLower, ssUpper)]
            where (NumericScoreRange lsLower lsUpper) = listeningScoreRange sg
                  (NumericScoreRange rsLower rsUpper) = readingScoreRange sg
                  (LetterScoreRange  wsLower wsUpper) = writingScoreRange sg
                  (LetterScoreRange  ssLower ssUpper) = speakingScoreRange sg
                  f :: Ord a => [(a, a, a)] -> [Int]
                  f = map (\(score, lower, upper) -> if score >= lower && score <= upper then 1 else 0)
