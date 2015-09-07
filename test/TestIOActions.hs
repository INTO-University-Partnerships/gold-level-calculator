{-# LANGUAGE TemplateHaskell #-}

module TestIOActions (testIOActions) where

import Util (ieltsLevelDataMap, CSVInputList(..))
import IOActions (getIELTSLevelDataMap, getCSVInputData)
import Types ()

import Data.Csv (encode)
import Data.Maybe (fromJust)
import Test.QuickCheck (Property, once)
import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Monadic (monadicIO, assert, run)
import System.Directory (removeFile)
import System.IO (openTempFile, hClose)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

prop_getIELTSLevelDataMap :: Property
prop_getIELTSLevelDataMap = once $ monadicIO $ do
    ieltsLevelDataMap' <- run $ getIELTSLevelDataMap "data/GOLD levels.csv"
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

return []

testIOActions :: IO Bool
testIOActions = $quickCheckAll
