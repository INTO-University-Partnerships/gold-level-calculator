{-# LANGUAGE OverloadedStrings #-}

module Calc (
    calcScoreTallys,
    calcTargetIndices,
    calcTargetIndex
) where

import Types (
    GroupName,
    ListeningScore,
    ReadingScore,
    WritingScore,
    SpeakingScore,
    NumericScoreRange(..),
    LetterScoreRange(..),
    DefaultToZero(..),
    ScoreGroup(..),
    ScoreGroupMap
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

zippingFunction :: (GroupName, ScoreGroup) -> (GroupName, Int) -> [Int]
zippingFunction (_, sg) (_, st) = V.toList $ V.findIndices (\(DefaultToZero count) -> count == st) cs
    where cs = counts sg

calcTargetIndices :: ScoreGroupMap -> ScoreTallyMap -> Maybe [[Int]]
calcTargetIndices msg mst =
    case compare (M.keys msg) (M.keys mst) of
        EQ -> Just $ zipWith zippingFunction (M.toAscList msg) (M.toAscList mst)
        _  -> Nothing

calcTargetIndex :: [[Int]] -> Maybe Int
calcTargetIndex xss =
    case f of
        [xs] -> if length xs == n then Just $ (head . head) f else Nothing
        _    -> Nothing
    where n = length xss
          f = filter (\l -> length l == n) $ (group . sort . concat) xss
