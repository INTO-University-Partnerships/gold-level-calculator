{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Types
import Control.Monad (mzero)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Attoparsec.Text as AT
import Data.Csv

numberOfCsvColumns :: Int
numberOfCsvColumns = 41

targetStartsAtColumn :: Int
targetStartsAtColumn = 7 -- zero-based

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
    return $ NumericScoreRange lower upper

parseLetterScoreRange :: AT.Parser LetterScoreRange
parseLetterScoreRange = do
    AT.skipSpace
    lower <- parseLetterScore
    AT.skipSpace
    AT.asciiCI "to"
    AT.skipSpace
    upper <- parseLetterScore
    AT.skipSpace
    return $ LetterScoreRange lower upper

parserMultipleColumns :: FromField a => Record -> [Int] -> Parser (V.Vector a)
parserMultipleColumns v xs = do
    ms <- mapM (index v) xs
    pure (V.fromList ms)

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
        | length v == numberOfCsvColumns = ScoreTarget <$>
                                           v .! 0      <*>
                                           parserMultipleColumns v [targetStartsAtColumn..numberOfCsvColumns-1]
        | otherwise = mzero

instance FromRecord ScoreGroup where
    parseRecord v
        | length v == numberOfCsvColumns = ScoreGroup <$>
                                           v .! 0     <*>
                                           v .! 1     <*>
                                           v .! 2     <*>
                                           v .! 3     <*>
                                           v .! 4     <*>
                                           v .! 5     <*>
                                           parserMultipleColumns v [targetStartsAtColumn..numberOfCsvColumns-1]
        | otherwise = mzero

type Matrix = V.Vector (V.Vector BL.ByteString)

parseWholeFile :: BL.ByteString -> Either String Matrix
parseWholeFile csvData = decode NoHeader csvData
