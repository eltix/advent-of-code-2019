{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Lib
  ( part1
  , part2
  , fuelRequired
  , recursiveFuelRequired
  ) where

import BasicPrelude
import qualified Data.Text as T
-- import Data.Graph
-- import Data.Map hiding (filter)
-- import qualified Data.Set as Set

fuelRequired ::
  Int  -- ^ the module mass
  -> Int  -- ^ the fuel requires
fuelRequired = max 0 . (flip (-) 2) . (`div` 3)

recursiveFuelRequired :: Int -> Int
recursiveFuelRequired = sum . tail . takeWhile (>0) . fuelSequence
  where
    fuelSequence :: Int -> [Int]
    fuelSequence n = n : (fuelSequence $ fuelRequired n)

readModuleMasses :: IO [Int]
readModuleMasses = fmap read . T.lines <$> readFile "day01/part1.csv"

part1 :: IO ()
part1 = do
  moduleMasses :: [Int] <- readModuleMasses
  let totalFuel = sum $ fuelRequired <$> moduleMasses
  putStrLn $ "total fuel = " ++ tshow totalFuel

part2 :: IO ()
part2 = do
  moduleMasses :: [Int] <- readModuleMasses
  let totalFuel = sum $ recursiveFuelRequired <$> moduleMasses
  putStrLn $ "total fuel = " ++ tshow totalFuel
