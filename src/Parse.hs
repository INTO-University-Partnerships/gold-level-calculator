{-# LANGUAGE OverloadedStrings #-}

module Parse (
    parseMatrix,
    toIELTSLevelDataMap
) where

import Types
import Control.Monad (mzero)
import Data.Csv (Parser, FromField, parseField, Record, FromRecord, parseRecord, runParser, index, (.!))
import Data.Text.Encoding (decodeUtf8)

import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

type Matrix = V.Vector (V.Vector BL.ByteString)

targetsStartAtColumn :: Int
targetsStartAtColumn = 7 -- zero-based

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
    AT.asciiCI "to"
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
    AT.asciiCI "to"
    AT.skipSpace
    upper <- parseLetterScore
    AT.skipSpace
    case compare lower upper of
        GT -> mzero
        _  -> return $ LetterScoreRange lower upper

parseMultipleColumns :: FromField a => Record -> [Int] -> Parser (V.Vector a)
parseMultipleColumns v xs = do
    ms <- mapM (index v) xs
    pure (V.fromList ms)

instance FromField DefaultToZero where
    parseField f = case runParser (parseField f) of
        Left  e -> pure $ DefaultToZero 0
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

parseMatrix :: Matrix -> (Either String (V.Vector ScoreTarget), Either String (V.Vector ScoreGroup))
parseMatrix m                   = (scoreTargets, scoreGroups)
    where m'                    = V.filter (\v -> not $ V.null v || BL.null (v V.! 0)) m
          bytesToVector bs      = runParser (parseRecord $ V.map BL.toStrict bs)
          potentialScoreTargets = V.filter (\v -> BL.null (v V.! 1)) m'
          scoreTargets          = V.mapM bytesToVector potentialScoreTargets :: Either String (V.Vector ScoreTarget)
          potentialScoreGroups  = V.filter (\v -> not $ BL.null (v V.! 1)) m'
          scoreGroups           = V.mapM bytesToVector potentialScoreGroups :: Either String (V.Vector ScoreGroup)

getScoreGroupMap :: IELTSLevel -> V.Vector ScoreGroup -> ScoreGroupMap
getScoreGroupMap l v = M.fromList $ V.toList v'
    where v' = V.map (\sg -> (scoreGroupName sg, sg)) $ V.filter (\sg -> scoreGroupLevel sg == l) v

getIELTSLevelData :: IELTSLevel -> V.Vector ScoreTarget -> V.Vector ScoreGroup -> IELTSLevelData
getIELTSLevelData l vst vsg = IELTSLevelData (V.head $ V.filter (\st -> scoreTargetLevel st == l) vst) (getScoreGroupMap l vsg)

toIELTSLevelDataMap :: V.Vector ScoreTarget -> V.Vector ScoreGroup -> IELTSLevelDataMap
toIELTSLevelDataMap vst vsg = M.fromList $ V.toList v
    where v = V.map (\st -> let stl = scoreTargetLevel st in (stl, getIELTSLevelData stl vst vsg)) vst
