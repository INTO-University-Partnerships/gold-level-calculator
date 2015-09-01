{-# LANGUAGE OverloadedStrings #-}

module IOActions (
    getIELTSLevelDataMap
) where

import Types (ScoreTarget, ScoreGroup, IELTSLevelDataMap)
import Parse (parseMatrix, toIELTSLevelDataMap)
import Data.Csv (decode, HasHeader(..))

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

csvDataFile :: String
csvDataFile = "data/GOLD levels.csv"

processMatrix :: (Either String (V.Vector ScoreTarget), Either String (V.Vector ScoreGroup))
                 -> IO (V.Vector ScoreTarget, V.Vector ScoreGroup)
processMatrix eithers = do
    case fst eithers of
        Left  evst -> showErrorAndReturnEmptyVectors evst
        Right rvst -> do
            case snd eithers of
                Left  evsg -> showErrorAndReturnEmptyVectors evsg
                Right rvsg -> return $ (rvst, rvsg)
    where
        showErrorAndReturnEmptyVectors e = do
            putStrLn e
            return $ (V.empty, V.empty)

getIELTSLevelDataMap :: IO (Maybe IELTSLevelDataMap)
getIELTSLevelDataMap = do
    csvData <- BL.readFile csvDataFile
    case decode NoHeader csvData of
        Left e -> do
            putStrLn e
            return Nothing
        Right m -> do
            (vst, vsg) <- (processMatrix . parseMatrix) m
            return $ Just $ toIELTSLevelDataMap vst vsg
