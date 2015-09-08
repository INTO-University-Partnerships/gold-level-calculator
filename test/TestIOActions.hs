{-# LANGUAGE TemplateHaskell #-}

module TestIOActions (testIOActions) where

import Util (ieltsLevelDataMap, CSVInputList(..))
import IOActions (getIELTSLevelDataMap, getCSVInputData, runOneCalculation)

import Types
    ( IELTSLevel
    , NumericScoreWrapper(..)
    , LetterScore
    , OneCalcOpts(..)
    , targetRange
    )

import Data.Csv (encode)
import Data.Maybe (fromJust)
import System.Directory (removeFile)
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
        (path, h) <- openTempFile "/tmp" "quickcheck.tmp"
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

return []

testIOActions :: IO Bool
testIOActions = $quickCheckAll
