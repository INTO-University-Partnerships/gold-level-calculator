{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

data IELTSLevel   = L45 | L50 | L55 | L60 | L65 deriving (Eq, Ord)
type GroupName    = String
type NumericScore = Int
data LetterScore  = A1 | A1P | A2 | A2P | B1 | B1P | B2 | B2P | C1 | C1P | C2 deriving (Eq, Ord)
data Target       = NoGOLD | L1 | L2 | L3 | Exception | Alert | Blank deriving Eq

data NumericScoreRange = NumericScoreRange NumericScore NumericScore deriving Eq
data LetterScoreRange  = LetterScoreRange  LetterScore  LetterScore  deriving Eq

newtype DefaultToZero = DefaultToZero Int deriving Eq

data ScoreTarget = ScoreTarget {
    scoreTargetLevel :: IELTSLevel,
    targets          :: V.Vector Target
} deriving Show

data ScoreGroup = ScoreGroup {
    scoreGroupLevel     :: IELTSLevel,
    scoreGroupName      :: GroupName,
    listeningScoreRange :: NumericScoreRange,
    readingScoreRange   :: NumericScoreRange,
    writingScoreRange   :: LetterScoreRange,
    speakingScoreRange  :: LetterScoreRange,
    counts              :: V.Vector DefaultToZero
} deriving Show

type ScoreGroupMap = M.Map GroupName ScoreGroup

data IELTSLevelData = IELTSLevelData {
    scoreTarget :: ScoreTarget,
    scoreGroups :: ScoreGroupMap
} deriving Show

type IELTSLevelDataMap = M.Map IELTSLevel IELTSLevelData

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

instance Show Target where
    show NoGOLD    = "No GOLD"
    show L1        = "L1"
    show L2        = "L2"
    show L3        = "L3"
    show Exception = "X"
    show Alert     = "Alert"
    show Blank     = ""

instance Show NumericScoreRange where
    show (NumericScoreRange lower upper) = show lower ++ " to " ++ show upper

instance Show LetterScoreRange where
    show (LetterScoreRange lower upper) = show lower ++ " to " ++ show upper

instance Show DefaultToZero where
    show (DefaultToZero 0) = ""
    show (DefaultToZero n) = show n
