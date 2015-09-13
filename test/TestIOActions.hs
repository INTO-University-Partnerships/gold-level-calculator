{-# LANGUAGE TemplateHaskell #-}

module TestIOActions (testIOActions) where

import Util (CSVInputList(..))
import IOActions (runOneCalculation, runManyCalculations)

import Types
    ( IELTSLevel
    , NumericScoreWrapper(..)
    , LetterScore
    , OneCalcOpts(..)
    , ManyCalcOpts(..)
    , targetRange
    )

import Data.Csv (encode)
import Data.List (intersperse, isInfixOf)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import System.FilePath (takeBaseName, takeExtension)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (openTempFile, hClose)
import System.IO.Silently (capture)
import Test.QuickCheck (Property, once)
import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import qualified Data.ByteString.Lazy as BL

csvDataFile :: String
csvDataFile = "data/GOLD levels.csv"

prop_runOneCalculation :: IELTSLevel -> NumericScoreWrapper -> NumericScoreWrapper -> LetterScore -> LetterScore -> Property
prop_runOneCalculation i (NumericScoreWrapper ls) (NumericScoreWrapper rs) ws ss = monadicIO $ do
    (captured, _) <- run $ capture $ runOneCalculation $ OneCalcOpts csvDataFile i ls rs ws ss
    assert $ elem captured $ map (\s -> show s ++ "\n") $ init targetRange

prop_runManyCalculationsSuccess :: CSVInputList -> Property
prop_runManyCalculationsSuccess (CSVInputList xs) = monadicIO $ do
    (captured, outputFile) <- run $ capture $ do
        dir <- getTemporaryDirectory
        (path, h) <- openTempFile dir "quickcheck.tmp"
        BL.hPut h $ encode xs
        hClose h
        _ <- runManyCalculations (ManyCalcOpts csvDataFile path)
        removeFile path
        let outputFile = takeBaseName path ++ "_output" ++ takeExtension path
        removeFile outputFile
        return outputFile
    assert $ captured == "Output file \"" ++ outputFile ++ "\" has been written to the current working directory\n"

prop_runManyCalculationsFail :: Property
prop_runManyCalculationsFail = once $ monadicIO $ do
    (captured, _) <- run $ capture $ do
        dir <- getTemporaryDirectory
        (path, h) <- openTempFile dir "quickcheck.tmp"
        BL.hPut h $ BL.fromStrict . encodeUtf8 . pack . concat . intersperse "\r\n" $
            [ ",,,,N,5,50,60,B1,B2+"
            , ",,,,Y,4.5,101,70,A1+,C2"
            , ",,,,N,6.5,80,80,C1,C2+"
            , ",,,,Y,4.5,32.5,40,A2,A2+"
            , ",,,,Yes,5.5,50,50,B2,B2"
            ]
        hClose h
        _ <- runManyCalculations (ManyCalcOpts csvDataFile path)
        removeFile path
    assert $ and $ map (\e -> e `isInfixOf` captured)
        [ "Row 1 has error \"\"5\" is not one of [4.5, 5.0, 5.5, 6.0, 6.5]\""
        , "Row 2 has error \"\"101\" is not an integer in the range [0..100] inclusive\""
        , "Row 3 has error \"\"C2+\" is not one of [A1, A1+, A2, A2+, B1, B1+, B2, B2+, C1, C1+, C2]\""
        , "Row 4 has error \"\"32.5\" is not an integer in the range [0..100] inclusive\""
        , "Row 5 has error \"\"Yes\" is not one of ['Y', 'N']\""
        ]

return []

testIOActions :: IO Bool
testIOActions = $quickCheckAll
