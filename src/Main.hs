{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parse
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    csvData <- BL.readFile "data/GOLD levels.csv"
    case parseWholeFile csvData of
        Left err -> putStrLn err
        Right v  -> V.forM_ v $ \r ->
            putStrLn $ show r
