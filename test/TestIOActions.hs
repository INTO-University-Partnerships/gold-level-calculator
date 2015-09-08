{-# LANGUAGE TemplateHaskell #-}

module TestIOActions (testIOActions) where

import Util (ieltsLevelDataMap, CSVInputList(..))
import IOActions (getIELTSLevelDataMap, getCSVInputData, runOneCalculation, runManyCalculations)

import Types
    ( IELTSLevel
    , NumericScoreWrapper(..)
    , LetterScore
    , OneCalcOpts(..)
    , ManyCalcOpts(..)
    , targetRange
    )

import Data.Csv (encode)
import Data.Maybe (fromJust)
import System.FilePath (takeBaseName, takeExtension)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (openTempFile, hClose)
import System.IO.Silently (capture)
import Test.QuickCheck (Property, once)
import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

csvDataFile :: String
csvDataFile = "data/GOLD levels.csv"

prop_getIELTSLevelDataMap :: Property
prop_getIELTSLevelDataMap = once $ monadicIO $ do
    ieltsLevelDataMap' <- run $ getIELTSLevelDataMap csvDataFile
    assert $ fromJust ieltsLevelDataMap' == ieltsLevelDataMap

prop_getCSVInputData :: CSVInputList -> Property
prop_getCSVInputData (CSVInputList xs) = monadicIO $ do
    csvInputData <- run $ do
        dir <- getTemporaryDirectory
        (path, h) <- openTempFile dir "quickcheck.tmp"
        BL.hPut h $ encode xs
        hClose h
        csvInputData <- getCSVInputData path
        removeFile path
        return csvInputData
    assert $ fromJust csvInputData == V.fromList xs

prop_runOneCalculation :: IELTSLevel -> NumericScoreWrapper -> NumericScoreWrapper -> LetterScore -> LetterScore -> Property
prop_runOneCalculation i (NumericScoreWrapper ls) (NumericScoreWrapper rs) ws ss = monadicIO $ do
    (captured, _) <- run $ capture $ runOneCalculation $ OneCalcOpts csvDataFile i ls rs ws ss
    assert $ elem captured $ map (\s -> show s ++ "\n") $ init targetRange

prop_runManyCalculations :: CSVInputList -> Property
prop_runManyCalculations (CSVInputList xs) = monadicIO $ do
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

return []

testIOActions :: IO Bool
testIOActions = $quickCheckAll
