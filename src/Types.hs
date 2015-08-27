{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Vector as V

data IELTSLevel   = L45 | L50 | L55 | L60 | L65 deriving (Eq, Ord)
type NumericScore = Int
data LetterScore  = A1 | A1P | A2 | A2P | B1 | B1P | B2 | B2P | C1 | C1P | C2 | C2P deriving (Eq, Ord)
data Target       = NoGOLD | L1 | L2 | L3 | Exception | Alert deriving Eq

data NumericScoreRange = NumericScoreRange NumericScore NumericScore deriving Show
data LetterScoreRange  = LetterScoreRange  LetterScore  LetterScore  deriving Show

data ScoreTarget = ScoreTarget {
    scoreTargetlevel :: IELTSLevel,
    targets          :: V.Vector Target
} deriving Show

data ScoreGroup = ScoreGroup {
    scoreGrouplevel     :: IELTSLevel,
    scoreGroupName      :: String,
    listeningScoreRange :: NumericScoreRange,
    readingScoreRange   :: NumericScoreRange,
    writingScoreRange   :: LetterScoreRange,
    speakingScoreRange  :: LetterScoreRange,
    matchCount          :: V.Vector Int
} deriving Show

instance Show IELTSLevel where
    show L45 = "4.5"
    show L50 = "5.0"
    show L55 = "5.5"
    show L60 = "6.0"
    show L65 = "6.5"

instance Show LetterScore where
    show A1  = "A1"
    show A1P = "A1+"
    show A2  = "A2"
    show A2P = "A2+"
    show B1  = "B1"
    show B1P = "B1+"
    show B2  = "B2"
    show B2P = "B2+"
    show C1  = "C1"
    show C1P = "C1+"
    show C2  = "C2"
    show C2P = "C2+"

instance Show Target where
    show NoGOLD    = "No GOLD"
    show L1        = "L1"
    show L2        = "L2"
    show L3        = "L3"
    show Exception = "X"
    show Alert     = "Alert"
