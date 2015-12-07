module Main where

import IOActions (runOneCalculation)
import OptParse (oneCalcOpts)

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
main = execParser opts >>= runOneCalculation
  where
  ourHeader   = "GOLD level calculator"
  ourProgDesc = "Given an IELTS level and (listening, reading, writing and speaking) scores, calculates one of [No GOLD, L1, L2, L3, X, Alert]"
  opts        = info (helper <*> oneCalcOpts) (fullDesc <> progDesc ourProgDesc <> header ourHeader)
