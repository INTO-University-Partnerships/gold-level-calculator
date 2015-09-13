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
    , BoolWrapper(..)
    , NumericScoreWrapper(..)
    , OneCalcOpts(..)
    , ManyCalcOpts(..)
    , GOLDCalcParams(..)
    , ScoreTarget(..)
    , ScoreGroup(..)
    , ScoreGroupMap
    , IELTSLevelData(..)
    , IELTSLevelDataMap
    , CSVInput(..)
    , CSVOutput(..)
    , enc
    , encShow
    , lookupIELTSLevel
    , targetsStartAtColumn
    , ieltsRange
    , targetRange
    , numericScoreRange
    , letterScoreRange
    , resultRange
    , parseNumericScore
    , parseLetterScore
) where

import Control.Monad (mzero, replicateM)
import Data.Csv (Parser, Record, FromField, parseField, FromRecord, ToRecord(..), parseRecord, record, runParser, (.!))
import Data.List (intersperse)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Test.QuickCheck (Arbitrary(..), elements, vectorOf)

import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

type Matrix            = V.Vector (V.Vector BL.ByteString)
type GroupName         = String
type NumericScore      = Int
data IELTSLevel        = L45 | L50 | L55 | L60 | L65 deriving (Eq, Ord)
data LetterScore       = A1 | A1P | A2 | A2P | B1 | B1P | B2 | B2P | C1 | C1P | C2 deriving (Eq, Ord)
data Target            = NoGOLD | L1 | L2 | L3 | X | Alert | Blank deriving Eq
type Result            = String
type ListeningScore    = NumericScore
type ReadingScore      = NumericScore
type WritingScore      = LetterScore
type SpeakingScore     = LetterScore
data NumericScoreRange = NumericScoreRange NumericScore NumericScore deriving Eq
data LetterScoreRange  = LetterScoreRange  LetterScore  LetterScore  deriving Eq
data OneCalcOpts       = OneCalcOpts FilePath IELTSLevel ListeningScore ReadingScore WritingScore SpeakingScore deriving Show
data ManyCalcOpts      = ManyCalcOpts FilePath FilePath deriving Show
data GOLDCalcParams    = GOLDCalcParams IELTSLevel NumericScoreWrapper NumericScoreWrapper WritingScore SpeakingScore deriving (Eq, Show)
data ScoreTarget       = ScoreTarget IELTSLevel (V.Vector Target) deriving (Eq, Show)

newtype DefaultToZero       = DefaultToZero Int deriving Eq
newtype BoolWrapper         = BoolWrapper Bool deriving Eq
newtype NumericScoreWrapper = NumericScoreWrapper NumericScore deriving (Eq, Ord)

data ScoreGroup = ScoreGroup {
    scoreGroupLevel     :: IELTSLevel
  , scoreGroupName      :: GroupName
  , listeningScoreRange :: NumericScoreRange
  , readingScoreRange   :: NumericScoreRange
  , writingScoreRange   :: LetterScoreRange
  , speakingScoreRange  :: LetterScoreRange
  , counts              :: V.Vector DefaultToZero
} deriving (Eq, Show)

type ScoreGroupMap     = M.Map GroupName ScoreGroup
data IELTSLevelData    = IELTSLevelData ScoreTarget ScoreGroupMap deriving (Eq, Show)
type IELTSLevelDataMap = M.Map IELTSLevel IELTSLevelData

data CSVInput = CSVInput {
    studentID :: String
  , lastName  :: String
  , firstName :: String
  , centre    :: String
  , prev      :: BoolWrapper
  , params    :: GOLDCalcParams
} deriving (Eq, Show)

data CSVOutput = CSVOutput CSVInput Result deriving (Eq, Show)

targetsStartAtColumn :: Int
targetsStartAtColumn = 7 -- zero-based

calcParamsStartAtColumn :: Int
calcParamsStartAtColumn = 5 -- zero-based

ieltsRange :: [IELTSLevel]
ieltsRange = [L45, L50, L55, L60, L65]

targetRange :: [Target]
targetRange = [NoGOLD, L1, L2, L3, X, Alert, Blank]

resultRange :: [Result]
resultRange = ["No GOLD", "X", "Alert", "GM1L1", "GM1L2", "GM1L3", "GM2L1", "GM2L2", "GM2L3"]

numericScoreRange :: [Int]
numericScoreRange = [0..100]

letterScoreRange :: [LetterScore]
letterScoreRange = [A1, A1P, A2, A2P, B1, B1P, B2, B2P, C1, C1P, C2]

enc :: String -> BI.ByteString
enc = encodeUtf8 . T.pack

encShow :: Show a => a -> BI.ByteString
encShow = enc . show

lookupIELTSLevel :: IELTSLevel -> IELTSLevelDataMap -> Either String IELTSLevelData
lookupIELTSLevel l m =
    case M.lookup l m of
        Nothing -> Left $ "IELTS level " ++ show l ++ " not found in IELTS level data map"
        Just ld -> Right ld

parseMultipleColumns :: FromField a => Record -> [Int] -> Parser (V.Vector a)
parseMultipleColumns v xs = do
    ms <- mapM ((.!) v) xs
    pure $ V.fromList ms

parseGOLDCalcParams :: Record -> [Int] -> Parser GOLDCalcParams
parseGOLDCalcParams v xs = do
    ielts  <- v .! head xs
    [l, r] <- mapM ((.!) v) $ take 2 (tail xs)
    [w, s] <- mapM ((.!) v) $ take 2 (drop 3 xs)
    pure $ GOLDCalcParams ielts l r w s

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
    show NoGOLD = "No GOLD"
    show L1     = "L1"
    show L2     = "L2"
    show L3     = "L3"
    show X      = "X"
    show Alert  = "Alert"
    show Blank  = ""

instance Show NumericScoreRange where
    show (NumericScoreRange lower upper) = show lower ++ " to " ++ show upper

instance Show LetterScoreRange where
    show (LetterScoreRange lower upper) = show lower ++ " to " ++ show upper

instance Show DefaultToZero where
    show (DefaultToZero 0) = ""
    show (DefaultToZero n) = show n

instance Show BoolWrapper where
    show (BoolWrapper True) = "Y"
    show (BoolWrapper _)    = "N"

instance Show NumericScoreWrapper where
    show (NumericScoreWrapper n) = show n

instance FromField DefaultToZero where
    parseField f = case runParser (parseField f) of
        Left  _ -> pure $ DefaultToZero 0
        Right n -> pure $ DefaultToZero n

instance FromField BoolWrapper where
    parseField f
        | f == "Y"  = pure $ BoolWrapper True
        | f == "y"  = pure $ BoolWrapper True
        | f == "N"  = pure $ BoolWrapper False
        | f == "n"  = pure $ BoolWrapper False
        | otherwise = fail $ "\"" ++ T.unpack (decodeUtf8 f) ++ "\" is not one of ['Y', 'N']"

instance FromField NumericScoreWrapper where
    parseField f = case AT.parseOnly (parseNumericScore <* AT.endOfInput) (decodeUtf8 f) of
        Right r -> pure $ NumericScoreWrapper r
        Left  _ -> fail $ "\"" ++ T.unpack (decodeUtf8 f) ++ "\" is not an integer in the range [0..100] inclusive"

instance FromField IELTSLevel where
    parseField f
        | f == "4.5" = pure L45
        | f == "5.0" = pure L50
        | f == "5.5" = pure L55
        | f == "6.0" = pure L60
        | f == "6.5" = pure L65
        | otherwise  = fail $ "\"" ++ T.unpack (decodeUtf8 f) ++ "\" is not one of [" ++ (concat $ intersperse ", " $ map show ieltsRange) ++ "]"

instance FromField Target where
    parseField f
        | f == "No GOLD" = pure NoGOLD
        | f == "L1"      = pure L1
        | f == "L2"      = pure L2
        | f == "L3"      = pure L3
        | f == "X"       = pure X
        | f == "Alert"   = pure Alert
        | f == ""        = pure Blank
        | otherwise      = fail $ "\"" ++ T.unpack (decodeUtf8 f) ++ "\" is not one of [" ++ (concat $ intersperse ", " $ map show $ init targetRange) ++ "]"

instance FromField LetterScore where
    parseField f = case AT.parseOnly (parseLetterScore <* AT.endOfInput) (decodeUtf8 f) of
        Right r -> pure r
        Left  _ -> fail $ "\"" ++ T.unpack (decodeUtf8 f) ++ "\" is not one of [" ++ (concat $ intersperse ", " $ map show letterScoreRange) ++ "]"

instance FromField LetterScoreRange where
    parseField f = case AT.parseOnly (parseLetterScoreRange <* AT.endOfInput) (decodeUtf8 f) of
        Right r -> pure r
        Left  _ -> fail $ "\"" ++ T.unpack (decodeUtf8 f) ++ "\" is not a valid letter score range"

instance FromField NumericScoreRange where
    parseField f = case AT.parseOnly (parseNumericScoreRange <* AT.endOfInput) (decodeUtf8 f) of
        Right r -> pure r
        Left  _ -> fail $ "\"" ++ T.unpack (decodeUtf8 f) ++ "\" is not a valid numeric score range"

instance FromRecord ScoreTarget where
    parseRecord v
        | l > targetsStartAtColumn = ScoreTarget
                                     <$> v .! 0
                                     <*> parseMultipleColumns v [targetsStartAtColumn..(l-1)]
        | otherwise = mzero
        where l = length v

instance FromRecord ScoreGroup where
    parseRecord v
        | l > targetsStartAtColumn = ScoreGroup
                                     <$> v .! 0
                                     <*> v .! 1
                                     <*> v .! 2
                                     <*> v .! 3
                                     <*> v .! 4
                                     <*> v .! 5
                                     <*> parseMultipleColumns v [targetsStartAtColumn..(l-1)]
        | otherwise = mzero
        where l = length v

instance FromRecord CSVInput where
    parseRecord v
        | l == 10 = CSVInput
                    <$> v .! 0
                    <*> v .! 1
                    <*> v .! 2
                    <*> v .! 3
                    <*> v .! 4
                    <*> parseGOLDCalcParams v [calcParamsStartAtColumn..(l-1)]
        | otherwise = mzero
        where l = length v

instance ToRecord CSVInput where
    toRecord (CSVInput stu las fir cen pre (GOLDCalcParams ielts ls rs ws ss)) = record l
        where
            l :: [BI.ByteString]
            l = map enc [stu, las, fir, cen] ++ [encShow pre] ++ [encShow ielts] ++ map encShow [ls, rs] ++ map encShow [ws, ss]

instance ToRecord CSVOutput where
    toRecord (CSVOutput csvInput result) = V.snoc (toRecord csvInput) (enc result)

instance Arbitrary BoolWrapper where
    arbitrary = do
        b <- elements [True, False]
        return $ BoolWrapper b

instance Arbitrary NumericScoreWrapper where
    arbitrary = elements $ map NumericScoreWrapper numericScoreRange

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

instance Arbitrary GOLDCalcParams where
    arbitrary = do
        ielts  <- arbitrary
        [l, r] <- replicateM 2 $ elements numericScoreRange
        [w, s] <- replicateM 2 arbitrary
        return $ GOLDCalcParams ielts (NumericScoreWrapper l) (NumericScoreWrapper r) w s

instance Arbitrary CSVInput where
    arbitrary = do
        stu <- vectorOf 10 $ elements ['0'..'9']
        las <- elements ["McGowan", "van Tienhoven", "Nockles", "van de Put", "Szkurłat", "Hodgson", "Elstob"]
        fir <- elements ["Mike", "Sacha", "Joe", "Thomas", "Marcin", "Max", "Anthony"]
        cen <- elements ["CIT", "EAL", "EXE", "MAN", "MDX", "MER", "NCL", "QUB", "SCO", "SGL", "STI", "UEA", "UOG"]
        pre <- arbitrary
        opt <- arbitrary
        return $ CSVInput stu las fir cen pre opt

instance Arbitrary CSVOutput where
    arbitrary = do
        i <- arbitrary
        r <- elements resultRange
        return $ CSVOutput i r
