{-# LANGUAGE OverloadedStrings #-}

module Types
( Matrix
, IELTSLevel(..)
, GroupName
, NumericScore
, LetterScore(..)
, Target(..)
, ListeningScore
, ReadingScore
, WritingScore
, SpeakingScore
, NumericScoreRange(..)
, LetterScoreRange(..)
, DefaultToZero(..)
, ScoreTarget(..)
, ScoreGroup(..)
, ScoreGroupMap
, IELTSLevelData(..)
, IELTSLevelDataMap
, ieltsRange
, targetRange
, numericScoreRange
, letterScoreRange
) where

import Control.Monad (mzero)
import Data.Csv (Parser, Record, FromField, parseField, FromRecord, parseRecord, runParser, index, (.!))
import Data.Text.Encoding (decodeUtf8)
import Test.QuickCheck (Arbitrary(..), elements)

import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

type Matrix            = V.Vector (V.Vector BL.ByteString)
data IELTSLevel        = L45 | L50 | L55 | L60 | L65 deriving (Eq, Ord)
type GroupName         = String
type NumericScore      = Int
data LetterScore       = A1 | A1P | A2 | A2P | B1 | B1P | B2 | B2P | C1 | C1P | C2 deriving (Eq, Ord)
data Target            = NoGOLD | L1 | L2 | L3 | Exception | Alert | Blank deriving Eq
type ListeningScore    = NumericScore
type ReadingScore      = NumericScore
type WritingScore      = LetterScore
type SpeakingScore     = LetterScore
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

targetsStartAtColumn :: Int
targetsStartAtColumn = 7 -- zero-based

ieltsRange :: [IELTSLevel]
ieltsRange = [L45, L50, L55, L60, L65]

targetRange :: [Target]
targetRange = [NoGOLD, L1, L2, L3, Exception, Alert, Blank]

numericScoreRange :: [Int]
numericScoreRange = [0..100]

letterScoreRange :: [LetterScore]
letterScoreRange = [A1, A1P, A2, A2P, B1, B1P, B2, B2P, C1, C1P, C2]

parseMultipleColumns :: FromField a => Record -> [Int] -> Parser (V.Vector a)
parseMultipleColumns v xs = do
    ms <- mapM (index v) xs
    pure (V.fromList ms)

parseNumericScore :: AT.Parser NumericScore
parseNumericScore = do
    n <- AT.decimal
    if n > 100 then mzero else return n

parseLetterScore :: AT.Parser LetterScore
parseLetterScore = do
    a <- AT.satisfy (\c -> c >= 'A' && c <= 'C')
    b <- AT.satisfy (\c -> c >= '1' && c <= '2')
    c <- AT.option ' ' $ AT.char '+'
    case f [a, b, c] of
        Just ls -> return ls
        Nothing -> mzero
    where f i
            | i == "A1 " = Just A1
            | i == "A1+" = Just A1P
            | i == "A2 " = Just A2
            | i == "A2+" = Just A2P
            | i == "B1 " = Just B1
            | i == "B1+" = Just B1P
            | i == "B2 " = Just B2
            | i == "B2+" = Just B2P
            | i == "C1 " = Just C1
            | i == "C1+" = Just C1P
            | i == "C2 " = Just C2
            | otherwise  = Nothing

parseNumericScoreRange :: AT.Parser NumericScoreRange
parseNumericScoreRange = do
    AT.skipSpace
    lower <- parseNumericScore
    AT.skipSpace
    _ <- AT.asciiCI "to"
    AT.skipSpace
    upper <- parseNumericScore
    AT.skipSpace
    case compare lower upper of
        GT -> mzero
        _  -> return $ NumericScoreRange lower upper

parseLetterScoreRange :: AT.Parser LetterScoreRange
parseLetterScoreRange = do
    AT.skipSpace
    lower <- parseLetterScore
    AT.skipSpace
    _ <- AT.asciiCI "to"
    AT.skipSpace
    upper <- parseLetterScore
    AT.skipSpace
    case compare lower upper of
        GT -> mzero
        _  -> return $ LetterScoreRange lower upper

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

instance FromField DefaultToZero where
    parseField f = case runParser (parseField f) of
        Left  _ -> pure $ DefaultToZero 0
        Right n -> pure $ DefaultToZero n

instance FromField IELTSLevel where
    parseField f
        | f == "4.5" = pure L45
        | f == "5.0" = pure L50
        | f == "5.5" = pure L55
        | f == "6.0" = pure L60
        | f == "6.5" = pure L65
        | otherwise  = mzero

instance FromField Target where
    parseField f
        | f == "No GOLD" = pure NoGOLD
        | f == "L1"      = pure L1
        | f == "L2"      = pure L2
        | f == "L3"      = pure L3
        | f == "X"       = pure Exception
        | f == "Alert"   = pure Alert
        | f == ""        = pure Blank
        | otherwise      = mzero

instance FromField LetterScoreRange where
    parseField f = case AT.parseOnly (parseLetterScoreRange <* AT.endOfInput) (decodeUtf8 f) of
        Right r -> pure r
        Left _  -> mzero

instance FromField NumericScoreRange where
    parseField f = case AT.parseOnly (parseNumericScoreRange <* AT.endOfInput) (decodeUtf8 f) of
        Right r -> pure r
        Left _  -> mzero

instance FromRecord ScoreTarget where
    parseRecord v
        | l > targetsStartAtColumn = ScoreTarget <$>
                                     v .! 0      <*>
                                     parseMultipleColumns v [targetsStartAtColumn..(l-1)]
        | otherwise = mzero
        where l = length v

instance FromRecord ScoreGroup where
    parseRecord v
        | l > targetsStartAtColumn = ScoreGroup <$>
                                     v .! 0     <*>
                                     v .! 1     <*>
                                     v .! 2     <*>
                                     v .! 3     <*>
                                     v .! 4     <*>
                                     v .! 5     <*>
                                     parseMultipleColumns v [targetsStartAtColumn..(l-1)]
        | otherwise = mzero
        where l = length v

instance Arbitrary IELTSLevel where
    arbitrary = elements ieltsRange

instance Arbitrary LetterScore where
    arbitrary = elements letterScoreRange

instance Arbitrary NumericScoreRange where
    arbitrary = do
        s1 <- elements numericScoreRange
        s2 <- elements numericScoreRange
        return $ NumericScoreRange (min s1 s2) (max s1 s2)

instance Arbitrary LetterScoreRange where
    arbitrary = do
        s1 <- elements letterScoreRange
        s2 <- elements letterScoreRange
        return $ LetterScoreRange (min s1 s2) (max s1 s2)

instance Arbitrary Target where
    arbitrary = elements targetRange
