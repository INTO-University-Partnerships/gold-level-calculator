module Parse
    ( parseScoreTarget
    , parseScoreGroup
    , toIELTSLevelDataMap
    , collectCSVInputData
    , collectCSVInputErrors
    ) where

import Types
    ( Matrix
    , ScoreTarget(..)
    , ScoreGroup(..)
    , ScoreGroupMap
    , IELTSLevel
    , IELTSLevelData(..)
    , IELTSLevelDataMap
    , CSVInput
    )

import Data.Csv (FromRecord, parseRecord, runParser)
import Data.Csv.Streaming (Records(..))

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv.Streaming as CS
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

filterIrrelevant :: Matrix -> Matrix
filterIrrelevant = V.filter (\v -> not $ V.null v || BL.null (v V.! 0))

bytesToVector :: FromRecord a => V.Vector BL.ByteString -> Either String a
bytesToVector bs = runParser (parseRecord $ V.map BL.toStrict bs)

parseScoreTarget :: Matrix -> Either String (V.Vector ScoreTarget)
parseScoreTarget m = V.mapM bytesToVector potentialScoreTargets
    where m' = filterIrrelevant m
          potentialScoreTargets = V.filter (\v -> BL.null (v V.! 1)) m'

parseScoreGroup :: Matrix -> Either String (V.Vector ScoreGroup)
parseScoreGroup m = V.mapM bytesToVector potentialScoreGroups
    where m' = filterIrrelevant m
          potentialScoreGroups = V.filter (\v -> not $ BL.null (v V.! 1)) m'

toIELTSLevelDataMap :: V.Vector ScoreTarget -> V.Vector ScoreGroup -> IELTSLevelDataMap
toIELTSLevelDataMap vst vsg = M.fromList . V.toList $ V.map (\(ScoreTarget l _) -> (l, getIELTSLevelData l)) vst
    where
        getIELTSLevelData :: IELTSLevel -> IELTSLevelData
        getIELTSLevelData l = IELTSLevelData (V.head $ V.filter (\(ScoreTarget lvl _) -> lvl == l) vst) scoreGroupMap
            where
                scoreGroupMap :: ScoreGroupMap
                scoreGroupMap = M.fromList . V.toList $ V.map (\sg -> (scoreGroupName sg, sg)) $ V.filter ((==) l . scoreGroupLevel) vsg

collectCSVInputData :: V.Vector CSVInput -> Records CSVInput -> V.Vector CSVInput
collectCSVInputData v (Nil _ _) = v
collectCSVInputData v (Cons r moreRecords) =
    case r of
        Right i -> collectCSVInputData (V.snoc v i) moreRecords
        Left  _ -> collectCSVInputData v moreRecords

collectCSVInputErrors :: Int -> V.Vector String -> CS.Records CSVInput -> Either String Bool
collectCSVInputErrors _ v (CS.Nil _ _)
    | l == 0    = Right True
    | otherwise = Left $ V.foldl1 (\acc x -> acc ++ "\r\n" ++ x) v
    where l     = V.length v
collectCSVInputErrors row v (CS.Cons r moreRecords) =
    case r of
        Right _ -> collectCSVInputErrors (row + 1) v moreRecords
        Left  e -> collectCSVInputErrors (row + 1) (V.snoc v $ "Row " ++ show row ++ " has error \"" ++ e ++ "\"") moreRecords
