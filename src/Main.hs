{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types (ScoreTarget, ScoreGroup, IELTSLevelData(..), IELTSLevelDataMap)
import Parse (parseMatrix, toIELTSLevelDataMap)
import Control.Monad (forM_)
import Data.Csv (decode, HasHeader(..))

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

processMatrix :: (Either String (V.Vector ScoreTarget), Either String (V.Vector ScoreGroup)) -> IO (V.Vector ScoreTarget, V.Vector ScoreGroup)
processMatrix eithers = do
    case fst eithers of
        Left e -> do
            putStrLn e
            return $ (V.empty, V.empty)
        Right r -> do
            case snd eithers of
                Left e' -> do
                    putStrLn e'
                    return $ (V.empty, V.empty)
                Right r' -> return $ (r, r')

getIELTSLevelDataMap :: IO (Maybe IELTSLevelDataMap)
getIELTSLevelDataMap = do
    csvData <- BL.readFile "data/GOLD levels.csv"
    case decode NoHeader csvData of
        Left  _ -> return Nothing
        Right m -> do
            (a, b) <- (processMatrix . parseMatrix) m
            return $ Just $ toIELTSLevelDataMap a b

main :: IO ()
main = do
    ldm <- getIELTSLevelDataMap
    case ldm of
        Nothing -> putStrLn "Something went wrong"
        Just ieltsLevelDataMap -> do
            forM_ (M.keys ieltsLevelDataMap) $ \k -> do
                case M.lookup k ieltsLevelDataMap of
                    Just (IELTSLevelData st sgm) -> do
                        putStrLn $ "IELTS level data for " ++ show k
                        putStrLn $ show st
                        putStrLn $ show sgm
                        putStrLn ""
