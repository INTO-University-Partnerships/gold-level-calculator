{-# LANGUAGE TemplateHaskell #-}

module TestParse (testParse) where

import Util (ieltsLevelDataMap)
import Parse (toIELTSLevelDataMap)
import Types (IELTSLevelData(..))

import Test.QuickCheck (Property, once)
import Test.QuickCheck.All (quickCheckAll)

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

prop_toIELTSLevelDataMap :: Property
prop_toIELTSLevelDataMap = once $ ieltsLevelDataMap' == ieltsLevelDataMap
  where
  vst = V.fromList $ M.elems $ M.map (\(IELTSLevelData st _) -> st) ieltsLevelDataMap
  vsg = V.fromList $ concat . M.elems $ M.map (\(IELTSLevelData _ msg) -> M.elems msg) ieltsLevelDataMap
  ieltsLevelDataMap' = toIELTSLevelDataMap vst vsg

return []

testParse :: IO Bool
testParse = $quickCheckAll
