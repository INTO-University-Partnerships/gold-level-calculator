{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types (IELTSLevelData(..))
import IOActions (getIELTSLevelDataMap)
import Control.Monad (forM_)

import qualified Data.Map.Strict as M

main :: IO ()
main = do
    ieltsLevelDataMap <- getIELTSLevelDataMap
    case ieltsLevelDataMap of
        Nothing -> putStrLn "Something went wrong"
        Just ieltsLevelDataMap' -> do
            forM_ (M.keys ieltsLevelDataMap') $ \k -> do
                case M.lookup k ieltsLevelDataMap' of
                    Nothing -> putStrLn $ "IELTS level " ++ show k ++ " not found in IELTS level data map"
                    Just (IELTSLevelData st msg) -> do
                        putStrLn $ "IELTS level data for " ++ show k
                        putStrLn $ show st
                        putStrLn $ show msg
                        putStrLn ""
