module Parse
  ( parseScoreTarget
  , parseScoreGroup
  , toIELTSLevelDataMap
  , collectCSVRecords
  ) where

import Types
  ( Matrix
  , ScoreTarget(..)
  , ScoreGroup(..)
  , ScoreGroupMap
  , IELTSLevel
  , IELTSLevelData(..)
  , IELTSLevelDataMap
  )

import Control.Monad.Writer (Writer, writer, runWriter, tell)
import Data.List (intersperse)
import Data.Csv (FromRecord, parseRecord, runParser)

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
  where
  m' = filterIrrelevant m
  potentialScoreTargets = V.filter (\v -> BL.null (v V.! 1)) m'

parseScoreGroup :: Matrix -> Either String (V.Vector ScoreGroup)
parseScoreGroup m = V.mapM bytesToVector potentialScoreGroups
  where
  m' = filterIrrelevant m
  potentialScoreGroups = V.filter (\v -> not $ BL.null (v V.! 1)) m'

toIELTSLevelDataMap :: V.Vector ScoreTarget -> V.Vector ScoreGroup -> IELTSLevelDataMap
toIELTSLevelDataMap vst vsg = M.fromList . V.toList $ V.map (\(ScoreTarget l _) -> (l, getIELTSLevelData l)) vst
  where
  getIELTSLevelData :: IELTSLevel -> IELTSLevelData
  getIELTSLevelData l = IELTSLevelData (V.head $ V.filter (\(ScoreTarget lvl _) -> lvl == l) vst) scoreGroupMap
    where
    scoreGroupMap :: ScoreGroupMap
    scoreGroupMap = M.fromList . V.toList $ V.map (\sg -> (scoreGroupName sg, sg)) $ V.filter ((==) l . scoreGroupLevel) vsg

collectCSVRecords :: CS.Records a -> Either String (V.Vector a)
collectCSVRecords rs = if length s == 0 then Right v' else Left s
  where
  (v', xs) = runWriter $ f rs 1 V.empty
  s = concat $ intersperse "\r\n" xs
  f :: CS.Records a -> Int -> V.Vector a -> Writer [String] (V.Vector a)
  f (CS.Nil _ _)       _   v = writer (v, [])
  f (CS.Cons (Right r) more) row v = f more (row + 1) $ V.snoc v r
  f (CS.Cons (Left e)  more) row v = do
    tell ["Row " ++ show row ++ " has error \"" ++ e ++ "\""]
    f more (row + 1) v
