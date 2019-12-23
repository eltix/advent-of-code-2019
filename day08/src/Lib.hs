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
import qualified Prelude
import qualified Data.Text as T

-- | Can't believe this isn't in the standard library :(
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = (take n l) : (chunksOf n (drop n l))

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
  image <- loadInput
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
  image <- loadInput
  let
    (width, height) = (25, 6)
    layerSize = width*height
    layers = chunksOf layerSize image
    image' = decodeImage (width, height) layers
  forM_ image' printRow

loadInput :: IO [Int]
loadInput = do
  s :: String <- init . T.unpack <$> readFile "day08/puzzle_input.csv"
  return $ Prelude.read <$> [[c] | c <- s]
