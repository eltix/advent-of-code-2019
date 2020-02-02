{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
  ( part1
  , part2
  ) where

import BasicPrelude

import Utils (chunksOf, loadIntList)

decodeImage :: (Int, Int) -> [[Int]] -> [[Int]]
decodeImage (width, height) layers = renderImage width pixels
  where
    n           = width*height
    pixelLayers = [[l !! i | l <- layers] | i <- [0..(n-1)]]
    pixels      = head . dropWhile (==2) <$> pixelLayers

renderImage :: Int -> [Int] -> [[Int]]
renderImage width pixels = chunksOf width pixels

printRow :: [Int] -> IO ()
printRow pixels = do
  forM_ pixels (\p -> putStr . f $ p)
  putStr "\n"
  where
    f = \case
      0 -> " "
      1 -> "*"
      _ -> "?"

part1 :: IO ()
part1 = do
  image <- loadIntList "day08/puzzle_input.csv"
  let
    (width, height) = (25, 6)
    layerSize = width*height
    layers = chunksOf layerSize image
    numOf n = length . filter (==n)
    fewest0L = fst . minimumBy (comparing snd) $ zip layers $ fmap (numOf 0) layers
    answer = numOf 1 fewest0L * numOf 2 fewest0L
  print $ answer

part2 :: IO ()
part2 = do
  image <- loadIntList "day08/puzzle_input.csv"
  let
    (width, height) = (25, 6)
    layerSize = width*height
    layers = chunksOf layerSize image
    image' = decodeImage (width, height) layers
  forM_ image' printRow
