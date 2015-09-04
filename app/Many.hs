module Main where

import Types (ManyCalcOpts(..))
import IOActions (runManyCalculations)

import Options.Applicative
    ( Parser
    , execParser
    , strOption
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

manyCalcOpts :: Parser ManyCalcOpts
manyCalcOpts = ManyCalcOpts
               <$> strOption (long "file"  <> short 'f' <> help "CSV data file mapping scores to targets")
               <*> strOption (long "users" <> short 'u' <> help "CSV users file with one row per user")

main :: IO ()
main = execParser opts >>= runManyCalculations
    where ourHeader   = "GOLD level calculator"
          ourProgDesc = "Calculates one of [No GOLD, GM1L1, GM1L2, GM1L3, GM2L1, GM2L2, GM2L3, X, Alert] for every row in a user file"
          opts        = info (helper <*> manyCalcOpts) (fullDesc <> progDesc ourProgDesc <> header ourHeader)
