{-# LANGUAGE OverloadedStrings #-}

module IOActions (getIELTSLevelDataMap, getCSVInputData, runOneCalculation, runManyCalculations) where

import Calc (calcTarget, calcManyTargets)
import Parse (parseCSVDataMatrix, toIELTSLevelDataMap, collectCSVInput)

import Types
    ( OneCalcOpts(..)
    , ManyCalcOpts(..)
    , ScoreTarget
    , ScoreGroup
    , IELTSLevelDataMap
    , CSVInput(..)
    )

import Data.Csv (decode, encode, HasHeader(..))
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName, takeExtension)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv.Streaming as CS
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

processCSVDataMatrix :: (Either String (V.Vector ScoreTarget), Either String (V.Vector ScoreGroup))
                        -> IO (V.Vector ScoreTarget, V.Vector ScoreGroup)
processCSVDataMatrix (st, sg) = do
    case st of
        Left  evst -> showErrorAndReturnEmptyVectors evst
        Right rvst -> do
            case sg of
                Left  evsg -> showErrorAndReturnEmptyVectors evsg
                Right rvsg -> return $ (rvst, rvsg)
    where
        showErrorAndReturnEmptyVectors e = do
            putStrLn e
            return $ (V.empty, V.empty)

getIELTSLevelDataMap :: FilePath -> IO (Maybe IELTSLevelDataMap)
getIELTSLevelDataMap f = do
    csvData <- BL.readFile f
    case decode NoHeader csvData of
        Left e -> do
            putStrLn e
            return Nothing
        Right m -> do
            (vst, vsg) <- (processCSVDataMatrix . parseCSVDataMatrix) m
            (return . Just) $ toIELTSLevelDataMap vst vsg

getCSVInputData :: FilePath -> IO (Maybe (V.Vector CSVInput))
getCSVInputData f = do
    csvData <- BL.readFile f
    let parsed = CS.decode NoHeader csvData :: CS.Records CSVInput
    errorCount <- showCSVInputErrors 1 0 parsed
    case errorCount of
        0 -> return . Just $ collectCSVInput V.empty parsed
        _ -> return Nothing
    where
        showCSVInputErrors :: Int -> Int -> CS.Records CSVInput -> IO Int
        showCSVInputErrors _   count (CS.Nil  _ _)           = return count
        showCSVInputErrors row count (CS.Cons r moreRecords) = do
            case r of
                Right _ -> showCSVInputErrors (row + 1) count moreRecords
                Left  e -> do
                    putStrLn $ "Row " ++ show row ++ " has error \"" ++ e ++ "\""
                    showCSVInputErrors (row + 1) (count + 1) moreRecords

runOneCalculation :: OneCalcOpts -> IO ()
runOneCalculation (OneCalcOpts f ielts ls rs ws ss) = do
    ieltsLevelDataMap <- getIELTSLevelDataMap f
    case ieltsLevelDataMap of
        Nothing -> putStrLn "Something went wrong trying to load or parse the CSV data file"
        Just ieltsLevelDataMap' -> do
            case M.lookup ielts ieltsLevelDataMap' of
                Nothing -> putStrLn $ "IELTS level " ++ show ielts ++ " not found in IELTS level data map"
                Just ld -> do
                    case calcTarget ld ls rs ws ss of
                        Nothing -> putStrLn "Something went wrong trying to calculate a score target"
                        Just t  -> putStrLn $ show t

generateOutputFile :: FilePath -> IELTSLevelDataMap -> V.Vector CSVInput -> IO ()
generateOutputFile f ieltsLevelDataMap csvInputData = do
    let outputFile = takeBaseName f ++ postfix ++ takeExtension f
    exists <- doesFileExist outputFile
    case exists of
        True  -> putStrLn $ "Output file \"" ++ outputFile ++ "\" already exists"
        False -> do
            let encoded = (encode . V.toList) $ calcManyTargets ieltsLevelDataMap csvInputData
            BL.writeFile (takeBaseName f ++ postfix ++ takeExtension f) encoded
            putStrLn $ "Output file \"" ++ outputFile ++ "\" has been written to the current working directory"
    where postfix = "_output"

runManyCalculations :: ManyCalcOpts -> IO ()
runManyCalculations (ManyCalcOpts f g) = do
    ieltsLevelDataMap <- getIELTSLevelDataMap f
    case ieltsLevelDataMap of
        Nothing -> putStrLn "Something went wrong trying to load or parse the CSV data file"
        Just ieltsLevelDataMap' -> do
            csvInputData <- getCSVInputData g
            case csvInputData of
                Nothing -> putStrLn "Something went wrong trying to load or parse the CSV users file"
                Just csvInputData' -> generateOutputFile g ieltsLevelDataMap' csvInputData'
