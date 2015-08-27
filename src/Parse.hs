{-# LANGUAGE OverloadedStrings #-}

module Parse where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv

type Matrix = V.Vector (V.Vector BL.ByteString)

parseWholeFile :: BL.ByteString -> Either String Matrix
parseWholeFile csvData = decode NoHeader csvData
