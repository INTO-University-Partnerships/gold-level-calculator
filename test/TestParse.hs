{-# LANGUAGE TemplateHaskell #-}

module TestParse (testParse) where

import Util (ScoreTargetWrapper(..), ScoreTargetList(..), ScoreGroupWrapper(..), ScoreGroupList(..), ieltsLevelDataMap)
import Parse (parseCSVDataMatrix, toIELTSLevelDataMap)
import Types (IELTSLevelData(..))

import Data.Csv (ToRecord, toRecord)
import Test.QuickCheck (Property, once)
import Test.QuickCheck.All (quickCheckAll)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

prop_parseCSVDataMatrix :: ScoreTargetList -> ScoreGroupList -> Bool
prop_parseCSVDataMatrix (ScoreTargetList sts) (ScoreGroupList sgs) =
    case evst of
        Left  _   -> False
        Right vst -> case evsg of
            Left  _   -> False
            Right vsg -> V.toList vst == map unwrapScoreTarget sts && V.toList vsg == map unwrapScoreGroup sgs
    where
        encode :: ToRecord a => [a] -> [V.Vector BL.ByteString]
        encode = map (V.map BL.fromStrict . toRecord)
        encodedScoreTargets = V.fromList $ encode sts
        encodedScoreGroups  = V.fromList $ encode sgs
        (evst, evsg)        = parseCSVDataMatrix $ encodedScoreTargets V.++ encodedScoreGroups

prop_toIELTSLevelDataMap :: Property
prop_toIELTSLevelDataMap = once $ ieltsLevelDataMap' == ieltsLevelDataMap
    where
        vst = V.fromList $ M.elems $ M.map (\(IELTSLevelData st _) -> st) ieltsLevelDataMap
        vsg = V.fromList $ (concat . M.elems) $ M.map (\(IELTSLevelData _ msg) -> M.elems msg) ieltsLevelDataMap
        ieltsLevelDataMap' = toIELTSLevelDataMap vst vsg

return []

testParse :: IO Bool
testParse = $quickCheckAll
