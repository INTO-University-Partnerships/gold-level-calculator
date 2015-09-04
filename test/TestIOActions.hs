{-# LANGUAGE TemplateHaskell #-}

module TestIOActions (testIOActions) where

import Util (ieltsLevelDataMap)
import IOActions (getIELTSLevelDataMap, getCSVInputData)

import Types
    ( IELTSLevel(..)
    , CSVInput(..)
    , GOLDCalcParams(..)
    , LetterScore(..)
    , BoolWrapper(..)
    , NumericScoreWrapper(..)
    )

import Data.Maybe (fromJust)
import Test.QuickCheck (Property, once)
import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import qualified Data.Vector as V

prop_getIELTSLevelDataMap :: Property
prop_getIELTSLevelDataMap = once $ monadicIO $ do
    ieltsLevelDataMap' <- run $ getIELTSLevelDataMap "data/GOLD levels.csv"
    assert $ fromJust ieltsLevelDataMap' == ieltsLevelDataMap

prop_getCSVInputData :: Property
prop_getCSVInputData  = once $ monadicIO $ do
    csvInputData <- run $ getCSVInputData "data/GOLD users.csv"
    assert $ fromJust csvInputData == V.fromList l
    where l =
            [ CSVInput "1231231230" "McGowan"       "Mike"  "Brighton" (BoolWrapper False) (GOLDCalcParams L55 (NumericScoreWrapper 50) (NumericScoreWrapper 60) B1  B2P)
            , CSVInput "4564564560" "van Tienhoven" "Sacha" "Brighton" (BoolWrapper True)  (GOLDCalcParams L45 (NumericScoreWrapper 40) (NumericScoreWrapper 70) A1P C2)
            , CSVInput "7897897890" "Nockles"       "Joe"   "Brighton" (BoolWrapper False) (GOLDCalcParams L65 (NumericScoreWrapper 80) (NumericScoreWrapper 80) C1  C2)
            ]

return []

testIOActions :: IO Bool
testIOActions = $quickCheckAll
