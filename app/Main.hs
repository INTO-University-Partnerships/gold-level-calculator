{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
    ( GOLDCalcOpts(..)
    , IELTSLevel(..)
    , NumericScore
    , LetterScore
    , parseNumericScore
    , parseLetterScore
    )

import IOActions (runCalculation)

import Options.Applicative
    ( Parser
    , ReadM
    , readerError
    , execParser
    , option
    , str
    , info
    , helper
    , progDesc
    , fullDesc
    , header
    , long
    , short
    , help
    , (<>)
    )

import qualified Data.Attoparsec.Text as AT
import qualified Data.Text as T

optParseIELTSLevel :: String -> ReadM IELTSLevel
optParseIELTSLevel f
    | f == "4.5" = return L45
    | f == "5.0" = return L50
    | f == "5.5" = return L55
    | f == "6.0" = return L60
    | f == "6.5" = return L65
    | otherwise  = readerError "Invalid IELTS level"

optParseNumericScore :: String -> ReadM NumericScore
optParseNumericScore f = do
    case AT.parseOnly (parseNumericScore <* AT.endOfInput) (T.pack f) of
        Right r -> return r
        Left  _ -> readerError "Invalid numeric score"

optParseLetterScore :: String -> ReadM LetterScore
optParseLetterScore f = do
    case AT.parseOnly (parseLetterScore <* AT.endOfInput) (T.pack f) of
        Right r -> return r
        Left  _ -> readerError "Invalid letter score"

goldCalcOpts :: Parser GOLDCalcOpts
goldCalcOpts = GOLDCalcOpts
               <$> option (str >>= optParseIELTSLevel)   (long "ielts"     <> short 'i' <> help "IELTS level, one of [4.5, 5.0, 5.5, 6.0, 6.5]")
               <*> option (str >>= optParseNumericScore) (long "listening" <> short 'l' <> help "Listening score in the range [0..100]")
               <*> option (str >>= optParseNumericScore) (long "reading"   <> short 'r' <> help "Reading score in the range [0..100]")
               <*> option (str >>= optParseLetterScore)  (long "writing"   <> short 'w' <> help "Writing score, one of [A1, A1+, A2, A2+, B1, B1+, B2, B2+, C1, C1+, C2]")
               <*> option (str >>= optParseLetterScore)  (long "speaking"  <> short 's' <> help "Speaking score, one of [A1, A1+, A2, A2+, B1, B1+, B2, B2+, C1, C1+, C2]")

main :: IO ()
main = execParser opts >>= runCalculation
    where ourHeader   = "GOLD level calculator"
          ourProgDesc = "Given an IELTS level and (listening, reading, writing and speaking) scores, calculates one of [No GOLD, L1, L2, L3, X, Alert]"
          opts        = info (helper <*> goldCalcOpts) (fullDesc <> progDesc ourProgDesc <> header ourHeader)
