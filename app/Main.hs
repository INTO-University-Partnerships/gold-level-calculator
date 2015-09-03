{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
    ( IELTSLevel(..)
    , NumericScore
    , ListeningScore
    , ReadingScore
    , LetterScore
    , WritingScore
    , SpeakingScore
    , parseNumericScore
    , parseLetterScore
    )

import IOActions (getIELTSLevelDataMap)
import Calc (calcTarget)

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
import qualified Data.Map.Strict as M
import qualified Data.Text as T

data GOLDCalcOpts = GOLDCalcOpts {
    level          :: IELTSLevel
  , listeningScore :: ListeningScore
  , readingScore   :: ReadingScore
  , writingScore   :: WritingScore
  , speakingScore  :: SpeakingScore
}

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

_main :: GOLDCalcOpts -> IO ()
_main (GOLDCalcOpts ielts ls rs ws ss) = do
    ieltsLevelDataMap <- getIELTSLevelDataMap
    case ieltsLevelDataMap of
        Nothing -> putStrLn "Something went wrong trying to load or parse the CSV data file"
        Just ieltsLevelDataMap' -> do
            case M.lookup ielts ieltsLevelDataMap' of
                Nothing -> putStrLn $ "IELTS level " ++ show ielts ++ " not found in IELTS level data map"
                Just ld -> do
                    case calcTarget ld ls rs ws ss of
                        Nothing -> putStrLn "Something went wrong trying to calculate a score target"
                        Just t  -> putStrLn $ show t

main :: IO ()
main = execParser opts >>= _main
    where ourHeader   = "GOLD level calculator"
          ourProgDesc = "Given an IELTS level and (listening, reading, writing and speaking) scores, calculates one of [No GOLD, L1, L2, L3, X, Alert]"
          opts        = info (helper <*> goldCalcOpts) (fullDesc <> progDesc ourProgDesc <> header ourHeader)
