module Main where

import IOActions (runManyCalculations)
import OptParse (manyCalcOpts)

import Options.Applicative
  ( execParser
  , info
  , helper
  , progDesc
  , fullDesc
  , header
  , (<>)
  )

main :: IO ()
main = execParser opts >>= runManyCalculations
  where
  ourHeader   = "GOLD level calculator"
  ourProgDesc = "Calculates one of [No GOLD, GM1L1, GM1L2, GM1L3, GM2L1, GM2L2, GM2L3, X, Alert] for every row in a user file"
  opts        = info (helper <*> manyCalcOpts) (fullDesc <> progDesc ourProgDesc <> header ourHeader)
