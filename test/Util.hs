module Util
( listsSummingToFour
, pentatopeNumbers
, utf8EncodedFieldData
, TargetList(..)
, DefaultToZeroList(..)
, CSVInputList(..)
, CSVInputListLong(..)
, ScoreTargetWrapper(..)
, ScoreTargetList(..)
, ScoreGroupWrapper(..)
, ScoreGroupList(..)
, ScoreTallys(..)
, ieltsLevelDataMap
) where

import Types
    ( Target
    , DefaultToZero(..)
    , IELTSLevel(..)
    , IELTSLevelData(..)
    , IELTSLevelDataMap
    , Target(..)
    , ScoreTarget(..)
    , LetterScore(..)
    , NumericScoreRange(..)
    , LetterScoreRange(..)
    , targetRange
    , ScoreGroup(..)
    , CSVInput(..)
    , targetsStartAtColumn
    , enc
    , encShow
    )

import Control.Monad (replicateM)
import Data.Csv (ToRecord(..), record)
import Data.Text.Encoding (encodeUtf8)
import Test.QuickCheck (Arbitrary(..), elements, choose, vectorOf, listOf)

import qualified Data.ByteString.Internal as BI
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

listsSummingToFour :: Int -> [[Int]]
listsSummingToFour n = filter (\xs -> sum xs == 4) $ zeroToFour n
    where
        zeroToFour :: Int -> [[Int]]
        zeroToFour 0 = [[]]
        zeroToFour m = concatMap (\xs -> [xs ++ [a] | a <- [0..4]]) $ zeroToFour (m-1)

pentatopeNumbers :: Int -> [Int]
pentatopeNumbers n = take n $ map (length . listsSummingToFour) [1..]

utf8EncodedFieldData :: Show a => a -> BI.ByteString
utf8EncodedFieldData = encodeUtf8 . T.pack . show

newtype TargetList         = TargetList [Target] deriving Show
newtype DefaultToZeroList  = DefaultToZeroList [DefaultToZero] deriving Show
newtype ScoreTallys        = ScoreTallys (IELTSLevel, [Int]) deriving Show
newtype CSVInputList       = CSVInputList [CSVInput] deriving Show
newtype CSVInputListLong   = CSVInputListLong [CSVInput] deriving Show
newtype ScoreTargetWrapper = ScoreTargetWrapper { unwrapScoreTarget :: ScoreTarget } deriving Show
newtype ScoreTargetList    = ScoreTargetList [ScoreTargetWrapper] deriving Show
newtype ScoreGroupWrapper  = ScoreGroupWrapper { unwrapScoreGroup :: ScoreGroup } deriving Show
newtype ScoreGroupList     = ScoreGroupList [ScoreGroupWrapper] deriving Show

instance Arbitrary TargetList where
    arbitrary = do
        l  <- elements $ pentatopeNumbers 5
        xs <- vectorOf l $ elements targetRange
        return $ TargetList xs

instance Arbitrary DefaultToZeroList where
    arbitrary = do
        let l = last $ pentatopeNumbers 5
        xs <- vectorOf l $ elements $ map DefaultToZero [0..4]
        return $ DefaultToZeroList xs

instance Arbitrary ScoreTallys where
    arbitrary = do
        n  <- choose (3, 4)
        xs <- elements $ listsSummingToFour n
        case n of
            3 -> return $ ScoreTallys (L45, xs)
            _ -> do
                l <- elements [L50, L55, L60, L65]
                return $ ScoreTallys (l, xs)

instance Arbitrary CSVInputList where
    arbitrary = do
        xs <- listOf arbitrary
        return $ CSVInputList xs

instance Arbitrary CSVInputListLong where
    arbitrary = do
        n  <- choose (500, 1000)
        xs <- vectorOf n $ arbitrary
        return $ CSVInputListLong xs

instance Arbitrary ScoreTargetWrapper where
    arbitrary = do
        ielts <- arbitrary
        (TargetList ts) <- arbitrary
        return $ ScoreTargetWrapper $ ScoreTarget ielts $ V.fromList ts

instance Arbitrary ScoreTargetList where
    arbitrary = do
        xs <- listOf arbitrary
        return $ ScoreTargetList xs

instance Arbitrary ScoreGroupWrapper where
    arbitrary = do
        ielts <- arbitrary
        n     <- elements ["Over", "Threshold", "Middle", "Deep"]
        [lsr, rsr] <- replicateM 2 arbitrary
        [wsr, ssr] <- replicateM 2 arbitrary
        (DefaultToZeroList cs) <- arbitrary
        return $ ScoreGroupWrapper $ ScoreGroup ielts n lsr rsr wsr ssr $ V.fromList cs

instance Arbitrary ScoreGroupList where
    arbitrary = do
        xs <- listOf arbitrary
        return $ ScoreGroupList xs

instance ToRecord ScoreTargetWrapper where
    toRecord (ScoreTargetWrapper (ScoreTarget ielts vt)) = record l
        where
            l :: [BI.ByteString]
            l = [encShow ielts] ++ map enc (replicate (targetsStartAtColumn - 1) "") ++ map encShow (V.toList vt)

instance ToRecord ScoreGroupWrapper where
    toRecord (ScoreGroupWrapper (ScoreGroup ielts n lsr rsr wsr ssr vc)) = record l
        where
            l :: [BI.ByteString]
            l = [encShow ielts] ++ [enc n] ++ map encShow [lsr, rsr] ++ map encShow [wsr, ssr] ++ [enc ""] ++ map encShow (V.toList vc)

l45LevelData :: (IELTSLevel, IELTSLevelData)
l45LevelData = (l, IELTSLevelData scoreTarget scoreGroupMap)
    where
        l             = L45
        over          = ("Over",           ScoreGroup l "Over"           (NumericScoreRange 40 100) (NumericScoreRange 40 100) (LetterScoreRange B1  C2)  (LetterScoreRange B1  C2) $
                        V.fromList $ map DefaultToZero [4,0,0,3,3,1,0,1,0,2,2,0,2,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        threshold     = ("Threshold - L1", ScoreGroup l "Threshold - L1" (NumericScoreRange 20  39) (NumericScoreRange 20  39) (LetterScoreRange A2  A2P) (LetterScoreRange A2  A2P) $
                        V.fromList $ map DefaultToZero [0,4,0,1,0,3,3,0,1,2,0,2,1,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        middle        = ("Middle",         ScoreGroup l "Middle"         (NumericScoreRange  0  19) (NumericScoreRange  0  19) (LetterScoreRange A1  A1P) (LetterScoreRange A1  A1P) $
                        V.fromList $ map DefaultToZero [0,0,4,0,1,0,1,3,3,0,2,2,1,1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        scoreTarget   = ScoreTarget l $ V.fromList $ [NoGOLD,L1,Alert,X,X,L1,L1,Alert,Alert,L1,X,L1,L1,L1,X] ++ replicate 20 Blank
        scoreGroupMap = M.fromList [over, threshold, middle]

l50LevelData :: (IELTSLevel, IELTSLevelData)
l50LevelData = (l, IELTSLevelData scoreTarget scoreGroupMap)
    where
        l             = L50
        over          = ("Over",           ScoreGroup l "Over"           (NumericScoreRange 50 100) (NumericScoreRange 50 100) (LetterScoreRange B1P C2)  (LetterScoreRange B1P C2) $
                        V.fromList $ map DefaultToZero [4,0,0,0,3,3,3,1,0,0,1,0,0,0,0,1,2,2,2,0,0,0,2,2,2,1,1,0,1,1,0,1,1,0,1])
        threshold     = ("Threshold - L2", ScoreGroup l "Threshold - L2" (NumericScoreRange 40  49) (NumericScoreRange 40  49) (LetterScoreRange B1  B1)  (LetterScoreRange B1  B1) $
                        V.fromList $ map DefaultToZero [0,4,0,0,1,0,0,3,3,3,0,1,0,0,1,0,2,0,0,2,2,0,1,1,0,2,2,2,1,0,1,1,0,1,1])
        middle        = ("Middle",         ScoreGroup l "Middle"         (NumericScoreRange 20  39) (NumericScoreRange 20  39) (LetterScoreRange A2  A2P) (LetterScoreRange A2  A2P) $
                        V.fromList $ map DefaultToZero [0,0,4,0,0,1,0,0,1,0,3,3,3,1,0,0,0,2,0,2,0,2,1,0,1,1,0,1,2,2,2,0,1,1,1])
        deep          = ("Deep",           ScoreGroup l "Deep"           (NumericScoreRange  0  19) (NumericScoreRange  0  19) (LetterScoreRange A1  A1P) (LetterScoreRange A1  A1P) $
                        V.fromList $ map DefaultToZero [0,0,0,4,0,0,1,0,0,1,0,0,1,3,3,3,0,0,2,0,2,2,0,1,1,0,1,1,0,1,1,2,2,2,1])
        scoreTarget   = ScoreTarget l $ V.fromList [NoGOLD,L2,L1,Alert,X,X,X,L2,L2,L2,L1,L1,L1,Alert,Alert,Alert,L2,L2,X,L2,X,L1,L2,L2,X,L2,L2,L2,L2,L1,L1,X,X,X,Alert]
        scoreGroupMap = M.fromList [over, threshold, middle, deep]

l55LevelData :: (IELTSLevel, IELTSLevelData)
l55LevelData = (l, IELTSLevelData scoreTarget scoreGroupMap)
    where
        l             = L55
        over          = ("Over",           ScoreGroup l "Over"           (NumericScoreRange 60 100) (NumericScoreRange 60 100) (LetterScoreRange B2  C2)  (LetterScoreRange B2  C2) $
                        V.fromList $ map DefaultToZero [4,0,0,0,3,3,3,1,0,0,1,0,0,0,0,1,2,2,2,0,0,0,2,2,2,1,1,0,1,1,0,1,1,0,1])
        threshold     = ("Threshold",      ScoreGroup l "Threshold"      (NumericScoreRange 40  59) (NumericScoreRange 40  59) (LetterScoreRange B1  B1P) (LetterScoreRange B1  B1P) $
                        V.fromList $ map DefaultToZero [0,4,0,0,1,0,0,3,3,3,0,1,0,0,1,0,2,0,0,2,2,0,1,1,0,2,2,2,1,0,1,1,0,1,1])
        middle        = ("Middle",         ScoreGroup l "Middle"         (NumericScoreRange 30  39) (NumericScoreRange 30  39) (LetterScoreRange A2P A2P) (LetterScoreRange A2P A2P) $
                        V.fromList $ map DefaultToZero [0,0,4,0,0,1,0,0,1,0,3,3,3,1,0,0,0,2,0,2,0,2,1,0,1,1,0,1,2,2,2,0,1,1,1])
        deep          = ("Deep",           ScoreGroup l "Deep"           (NumericScoreRange  0  29) (NumericScoreRange  0  29) (LetterScoreRange A1  A2)  (LetterScoreRange A1  A2) $
                        V.fromList $ map DefaultToZero [0,0,0,4,0,0,1,0,0,1,0,0,1,3,3,3,0,0,2,0,2,2,0,1,1,0,1,1,0,1,1,2,2,2,1])
        scoreTarget   = ScoreTarget l $ V.fromList [NoGOLD,L2,L1,X,X,X,X,L2,L2,L2,L1,L1,L1,X,X,X,L2,L2,X,L2,X,L1,L2,L2,X,L2,L2,L2,L2,L1,L1,X,X,X,Alert]
        scoreGroupMap = M.fromList [over, threshold, middle, deep]

l60LevelData :: (IELTSLevel, IELTSLevelData)
l60LevelData = (l, IELTSLevelData scoreTarget scoreGroupMap)
    where
        l             = L60
        over          = ("Over",           ScoreGroup l "Over"           (NumericScoreRange 67 100) (NumericScoreRange 67 100) (LetterScoreRange B2P C2)  (LetterScoreRange B2P C2) $
                        V.fromList $ map DefaultToZero [4,0,0,0,3,3,3,1,0,0,1,0,0,0,0,1,2,2,2,0,0,0,2,2,2,1,1,0,1,1,0,1,1,0,1])
        threshold     = ("Threshold",      ScoreGroup l "Threshold"      (NumericScoreRange 60  66) (NumericScoreRange 60  66) (LetterScoreRange B2  B2)  (LetterScoreRange B2  B2) $
                        V.fromList $ map DefaultToZero [0,4,0,0,1,0,0,3,3,3,0,1,0,0,1,0,2,0,0,2,2,0,1,1,0,2,2,2,1,0,1,1,0,1,1])
        middle        = ("Middle",         ScoreGroup l "Middle"         (NumericScoreRange 50  59) (NumericScoreRange 50  59) (LetterScoreRange B1P B1P) (LetterScoreRange B1P B1P) $
                        V.fromList $ map DefaultToZero [0,0,4,0,0,1,0,0,1,0,3,3,3,1,0,0,0,2,0,2,0,2,1,0,1,1,0,1,2,2,2,0,1,1,1])
        deep          = ("Deep",           ScoreGroup l "Deep"           (NumericScoreRange  0  49) (NumericScoreRange  0  49) (LetterScoreRange A1  B1)  (LetterScoreRange A1  B1) $
                        V.fromList $ map DefaultToZero [0,0,0,4,0,0,1,0,0,1,0,0,1,3,3,3,0,0,2,0,2,2,0,1,1,0,1,1,0,1,1,2,2,2,1])
        scoreTarget   = ScoreTarget l $ V.fromList [NoGOLD,L3,L2,X,X,X,X,L3,L3,L3,L2,L2,L2,X,X,X,L3,L3,X,L3,X,L2,L3,L3,X,L3,L3,L3,L3,L2,L2,X,X,X,Alert]
        scoreGroupMap = M.fromList [over, threshold, middle, deep]

l65LevelData :: (IELTSLevel, IELTSLevelData)
l65LevelData = (l, IELTSLevelData scoreTarget scoreGroupMap)
    where
        l             = L65
        over          = ("Over",           ScoreGroup l "Over"           (NumericScoreRange 76 100) (NumericScoreRange 76 100) (LetterScoreRange C1  C2)  (LetterScoreRange C1  C2) $
                        V.fromList $ map DefaultToZero [4,0,0,0,3,3,3,1,0,0,1,0,0,0,0,1,2,2,2,0,0,0,2,2,2,1,1,0,1,1,0,1,1,0,1])
        threshold     = ("Threshold",      ScoreGroup l "Threshold"      (NumericScoreRange 67  75) (NumericScoreRange 67  75) (LetterScoreRange B2P B2P) (LetterScoreRange B2P B2P) $
                        V.fromList $ map DefaultToZero [0,4,0,0,1,0,0,3,3,3,0,1,0,0,1,0,2,0,0,2,2,0,1,1,0,2,2,2,1,0,1,1,0,1,1])
        middle        = ("Middle",         ScoreGroup l "Middle"         (NumericScoreRange 60  66) (NumericScoreRange 60  66) (LetterScoreRange B2  B2)  (LetterScoreRange B2  B2) $
                        V.fromList $ map DefaultToZero [0,0,4,0,0,1,0,0,1,0,3,3,3,1,0,0,0,2,0,2,0,2,1,0,1,1,0,1,2,2,2,0,1,1,1])
        deep          = ("Deep",           ScoreGroup l "Deep"           (NumericScoreRange  0  59) (NumericScoreRange  0  59) (LetterScoreRange A1  B1P) (LetterScoreRange A1  B1P) $
                        V.fromList $ map DefaultToZero [0,0,0,4,0,0,1,0,0,1,0,0,1,3,3,3,0,0,2,0,2,2,0,1,1,0,1,1,0,1,1,2,2,2,1])
        scoreTarget   = ScoreTarget l $ V.fromList [NoGOLD,L3,L3,X,X,X,X,L3,L3,L3,L3,L3,L3,X,X,X,L3,L3,X,L3,X,L3,L3,L3,X,L3,L3,L3,L3,L3,L3,X,X,X,Alert]
        scoreGroupMap = M.fromList [over, threshold, middle, deep]

ieltsLevelDataMap :: IELTSLevelDataMap
ieltsLevelDataMap = M.fromList [l45LevelData, l50LevelData, l55LevelData, l60LevelData, l65LevelData]
