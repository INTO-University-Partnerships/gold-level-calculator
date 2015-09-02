{-# LANGUAGE OverloadedStrings #-}

module Parse (
    parseMatrix,
    toIELTSLevelDataMap
) where

import Types (Matrix, IELTSLevel, ScoreTarget(..), ScoreGroup(..), ScoreGroupMap, IELTSLevelData(..), IELTSLevelDataMap)
import Data.Csv (parseRecord, runParser)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

parseMatrix :: Matrix -> (Either String (V.Vector ScoreTarget), Either String (V.Vector ScoreGroup))
parseMatrix m                   = (actualScoreTargets, actualScoreGroups)
    where m'                    = V.filter (\v -> not $ V.null v || BL.null (v V.! 0)) m
          bytesToVector bs      = runParser (parseRecord $ V.map BL.toStrict bs)
          potentialScoreTargets = V.filter (\v -> BL.null (v V.! 1)) m'
          actualScoreTargets    = V.mapM bytesToVector potentialScoreTargets :: Either String (V.Vector ScoreTarget)
          potentialScoreGroups  = V.filter (\v -> not $ BL.null (v V.! 1)) m'
          actualScoreGroups     = V.mapM bytesToVector potentialScoreGroups :: Either String (V.Vector ScoreGroup)

getScoreGroupMap :: IELTSLevel -> V.Vector ScoreGroup -> ScoreGroupMap
getScoreGroupMap l v = M.fromList $ V.toList v'
    where v' = V.map (\sg -> (scoreGroupName sg, sg)) $ V.filter (\sg -> scoreGroupLevel sg == l) v

getIELTSLevelData :: IELTSLevel -> V.Vector ScoreTarget -> V.Vector ScoreGroup -> IELTSLevelData
getIELTSLevelData l vst vsg = IELTSLevelData (V.head $ V.filter (\st -> scoreTargetLevel st == l) vst) (getScoreGroupMap l vsg)

toIELTSLevelDataMap :: V.Vector ScoreTarget -> V.Vector ScoreGroup -> IELTSLevelDataMap
toIELTSLevelDataMap vst vsg = M.fromList $ V.toList v
    where v = V.map (\st -> let stl = scoreTargetLevel st in (stl, getIELTSLevelData stl vst vsg)) vst
