{-# LANGUAGE OverloadedStrings #-}

module IOActions (getIELTSLevelDataMap, runCalculation) where

import Calc (calcTarget)
import Types (GOLDCalcOpts(..), ScoreTarget, ScoreGroup, IELTSLevelDataMap)
import Parse (parseMatrix, toIELTSLevelDataMap)
import Data.Csv (decode, HasHeader(..))

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
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

runCalculation :: GOLDCalcOpts -> IO ()
runCalculation (GOLDCalcOpts ielts ls rs ws ss) = do
    ieltsLevelDataMap <- getIELTSLevelDataMap
    case ieltsLevelDataMap of
        Nothing -> putStrLn "Something went wrong trying to load or parse the CSV data file"
        Just ieltsLevelDataMap' -> do
            case M.lookup ielts ieltsLevelDataMap' of
                Nothing -> putStrLn $ "IELTS level " ++ show ielts ++ " not found in IELTS level data map"
                Just ld -> do
                    case calcTarget ld ls rs ws ss of
                        Nothing -> putStrLn "Something went wrong trying to calculate a score target"
                        Just t  -> putStrLn $ show t
