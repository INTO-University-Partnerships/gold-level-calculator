{-# LANGUAGE OverloadedStrings #-}

module IOActions
    ( runOneCalculation
    , runManyCalculations
    ) where

import Calc (calcTarget, calcManyTargets)

import Parse
    ( parseScoreTarget
    , parseScoreGroup
    , toIELTSLevelDataMap
    , collectCSVInputData
    , collectCSVInputErrors
    )

import Types
    ( Target
    , OneCalcOpts(..)
    , ManyCalcOpts(..)
    , IELTSLevelDataMap
    , CSVInput(..)
    , lookupIELTSLevel
    )

import Data.Csv (decode, encodeWith, defaultEncodeOptions, EncodeOptions(..), Quoting(..), HasHeader(..))
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName, takeExtension)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv.Streaming as CS
import qualified Data.Vector as V

generateOutputFile :: FilePath -> IELTSLevelDataMap -> V.Vector CSVInput -> IO ()
generateOutputFile f ieltsLevelDataMap csvInputData = do
    exists <- doesFileExist outputFile
    case exists of
        True  -> putStrLn $ "Output file \"" ++ outputFile ++ "\" already exists"
        False -> do
            let encoded = encodeWith encodeOpts . V.toList $ calcManyTargets ieltsLevelDataMap csvInputData
            BL.writeFile outputFile encoded
            putStrLn $ "Output file \"" ++ outputFile ++ "\" has been written to the current working directory"
            where encodeOpts = defaultEncodeOptions { encQuoting = QuoteAll }
    where postfix    = "_output"
          outputFile = takeBaseName f ++ postfix ++ takeExtension f

runOneCalculation :: OneCalcOpts -> IO ()
runOneCalculation (OneCalcOpts f ielts ls rs ws ss) = do
    csvData <- BL.readFile f
    case h csvData of
        Left  e -> putStrLn e
        Right t -> putStrLn $ show t
    where
        h :: BL.ByteString -> Either String Target
        h csvData = do
            m   <- decode NoHeader csvData
            vst <- parseScoreTarget m
            vsg <- parseScoreGroup m
            ld  <- lookupIELTSLevel ielts $ toIELTSLevelDataMap vst vsg
            calcTarget ld ls rs ws ss

runManyCalculations :: ManyCalcOpts -> IO ()
runManyCalculations (ManyCalcOpts f g) = do
    csvData1 <- BL.readFile f
    csvData2 <- BL.readFile g
    case h csvData1 csvData2 of
        Left e -> putStrLn e
        Right (ieltsLevelDataMap, csvInputData) -> generateOutputFile g ieltsLevelDataMap csvInputData
    where
        h :: BL.ByteString -> BL.ByteString -> Either String (IELTSLevelDataMap, V.Vector CSVInput)
        h csvData1 csvData2 = do
            m   <- decode NoHeader csvData1
            vst <- parseScoreTarget m
            vsg <- parseScoreGroup m
            _   <- collectCSVInputErrors 1 V.empty csvInputData
            return (toIELTSLevelDataMap vst vsg, collectCSVInputData V.empty csvInputData)
            where csvInputData = CS.decode NoHeader csvData2
