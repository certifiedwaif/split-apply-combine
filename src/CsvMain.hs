{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Either
import Data.HashMap as M
import qualified Data.Vector as V
import Prelude

main :: IO ()
main
  -- datestamp, fieldname, value
 = do
  let dateStampToValue :: M.Map String Double = M.empty
  csvData <- BL.readFile "data.csv"
  case decode HasHeader csvData of
    Left err -> putStrLn err
    Right v -- You could use a fold over v here
     -> do
      let vl = V.toList v
          vm = buildMaps vl
          keys = keys vm
      map (\k -> mean $ vm ! k) keys
  where
    mean l = sum l / fromIntegral . length l
       -- For each key tuple, calculate the mean
      -- V.forM_ v $ \(datestamp :: String, fieldname :: String, value :: Double) ->
      --   putStrLn $ datestamp ++ " " ++ fieldname ++ " " ++ show value
  -- Split-apply-combine pattern
  -- How do you have a program do something every minute?
  -- sleep
  -- fork a new thread
  -- access variables shared between threads - MVar
  -- connection pools
  --
  -- Create a program that creates two threads, gets a stream of integers
  -- and then gets their maximum
  -- Parallelism and Concurrency in Haskell

type MyMap = M.Map (String, String) [Double]

buildMaps :: [(String, String, Double)] -> MyMap
buildMaps [(datestamp, field, value)]
  -- This isn't going to work, because the Map values aren't Lists.
  -- I think I need a fold.
 =
  let emptyMap :: MyMap = M.empty
   in foldl addOne emptyMap $ map (\a b c -> ((a, b), c))
  where
    addOne :: MyMap -> ((String, String), Double) -> MyMap
    addOne map ((a, b), c) =
      if M.member map (a, b)
        then M.insert map (a, b) (map ! (a, b) : c)
        else M.insert map (a, b) []
