module Parse
( parseCSVDataMatrix
, toIELTSLevelDataMap
, collectCSVInput
) where

import Types
    ( Matrix
    , ScoreTarget(..)
    , ScoreGroup(..)
    , IELTSLevelData(..)
    , IELTSLevelDataMap
    , CSVInput
    )

import Data.Csv (parseRecord, runParser)
import Data.Csv.Streaming (Records(..))

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

parseCSVDataMatrix :: Matrix -> (Either String (V.Vector ScoreTarget), Either String (V.Vector ScoreGroup))
parseCSVDataMatrix m = (actualScoreTargets, actualScoreGroups)
    where m' = V.filter (\v -> not $ V.null v || BL.null (v V.! 0)) m
          bytesToVector bs      = runParser (parseRecord $ V.map BL.toStrict bs)
          potentialScoreTargets = V.filter (\v -> BL.null (v V.! 1)) m'
          actualScoreTargets    = V.mapM bytesToVector potentialScoreTargets :: Either String (V.Vector ScoreTarget)
          potentialScoreGroups  = V.filter (\v -> not $ BL.null (v V.! 1)) m'
          actualScoreGroups     = V.mapM bytesToVector potentialScoreGroups :: Either String (V.Vector ScoreGroup)

toIELTSLevelDataMap :: V.Vector ScoreTarget -> V.Vector ScoreGroup -> IELTSLevelDataMap
toIELTSLevelDataMap vst vsg = M.fromList $ V.toList $ V.map (\(ScoreTarget l _) -> (l, getIELTSLevelData l)) vst
    where getIELTSLevelData l = IELTSLevelData (V.head $ V.filter (\(ScoreTarget lvl _) -> lvl == l) vst) scoreGroupMap
            where scoreGroupMap = M.fromList $ V.toList $ V.map (\sg -> (scoreGroupName sg, sg)) $ V.filter (\sg -> scoreGroupLevel sg == l) vsg

collectCSVInput :: V.Vector CSVInput -> Records CSVInput -> V.Vector CSVInput
collectCSVInput v (Nil _ _) = v
collectCSVInput v (Cons e moreRecords) =
    case e of
        Right i -> collectCSVInput (V.snoc v i) moreRecords
        Left  _ -> collectCSVInput v moreRecords
