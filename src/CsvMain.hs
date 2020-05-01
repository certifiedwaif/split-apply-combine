{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List (map, foldl)
import Data.Map (Map, member, insert, empty, (!), keys)
import qualified Data.Vector as V

type MyMap = Map (String, String) [Double]

main :: IO ()
main
  -- datestamp, fieldname, value
 = do
  csvData <- BL.readFile "data.csv"
  case decode HasHeader csvData of
    Left err -> putStrLn err
    Right v -- You could use a fold over v here
     -> do
      let vl = V.toList v
          vm = buildMaps vl
          vmKeys = keys vm
      putStrLn $ show vm
      putStrLn $ show $ map (\k -> (k, mean $ vm ! k)) vmKeys
  where
    mean :: [Double] -> Double
    mean l = sum l / (fromIntegral $ length l)

buildMaps :: [(String, String, Double)] -> MyMap
buildMaps l
 =
  let emptyMap :: MyMap = empty
   in foldl addOne emptyMap $ map (\(a, b, c) -> ((a, b), c)) l
  where
    addOne :: MyMap -> ((String, String), Double) -> MyMap
    addOne map' ((a, b), c) =
      if member (a, b) map'
        then
          let newValue :: [Double] =  c : (map' ! (a, b))
          in
          insert (a, b) newValue map'
        else
          insert (a, b) [c] map'