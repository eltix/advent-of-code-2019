{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( part1
  , part2
  ) where

import BasicPrelude
import qualified Data.Text as T
import qualified Data.Set as Set

type Position = (Int, Int)
type Path = [Position]

manhattanDistance :: Position -> Position -> Int
manhattanDistance (xa, ya) (xb, yb) = abs (xa - xb) + abs (ya - yb)

data Direction = U | R | D | L
  deriving (Show)

data Jump =
  Jump
  { direction :: Direction
  , len       :: Int
  } deriving (Show)

fromText :: Text -> Jump
fromText t =
  let len = read . T.tail $ t :: Int
  in case T.head t of
    'U' -> Jump{direction = U , ..}
    'R' -> Jump{direction = R , ..}
    'D' -> Jump{direction = D , ..}
    'L' -> Jump{direction = L , ..}
    d   -> error $ "Unknown direction: " ++ show [d]

type Wire = [Jump]

wirePath :: Wire -> Path
-- we use tail to remove the central port (0,0) from the list
wirePath = tail . foldl' addPoints [(0,0)]
  where
    addPoints path (Jump d l) = path ++ (drawPath d l $ last path)
    drawPath d l (x, y) = case d of
      U -> [(x  , y+i) | i <- [1..l]]
      R -> [(x+i, y  ) | i <- [1..l]]
      D -> [(x  , y-i) | i <- [1..l]]
      L -> [(x-i, y  ) | i <- [1..l]]

findClosestIntersection :: Wire -> Wire -> (Position, Int)
findClosestIntersection w1 w2 = (closest, distance)
  where
    intersections         = findIntersections (wirePath w1) (wirePath w2)
    positionsAndDistances = [(p, manhattanDistance (0,0) p) | p <- intersections]
    (closest, distance)   = minimumBy (comparing snd) positionsAndDistances

findQuickestIntersection :: Wire -> Wire -> (Position, Int)
findQuickestIntersection w1 w2 = (quickest, distance)
  where
    (path1, path2)        = (wirePath w1, wirePath w2)
    intersections         = findIntersections path1 path2
    delaysOnPath1         = delayOnPath path1 <$> intersections
    delaysOnPath2         = delayOnPath path2 <$> intersections
    totalDelays           = zipWith (+) delaysOnPath1 delaysOnPath2
    positionsAndDistances = zip intersections totalDelays
    (quickest, distance)  = minimumBy (comparing snd) positionsAndDistances

    delayOnPath :: Path -> Position -> Int
    -- we add one to account for the fact that we removed the central port (0,0)
    -- from the path in 'wirePath'. Come to think of it, it may not have been a
    -- great idea.
    delayOnPath path intersection = (+) 1 $ length $ takeWhile (/= intersection) path


findIntersections :: Path -> Path -> [Position]
findIntersections path1 path2 = Set.toList $ Set.intersection pointSet1 pointSet2
  where
    pointSet1 = Set.fromList path1
    pointSet2 = Set.fromList path2


part1 :: IO ()
part1 = do
  (wire1, wire2) <- readInput
  let
    (closest, distance) = findClosestIntersection wire1 wire2
  putStrLn $ "Closest intersection: " ++ tshow closest
  putStrLn $ "Manhattan distance = " ++ tshow distance

part2 :: IO ()
part2 = do
  (wire1, wire2) <- readInput
  let
    (quickest, delay) = findQuickestIntersection wire1 wire2
  putStrLn $ "Quickest intersection: " ++ tshow quickest
  putStrLn $ "Signal delay = " ++ tshow delay

readInput :: IO (Wire, Wire)
readInput = do
  input <- fmap (T.splitOn ",") . T.lines <$> readFile "day03/part1.csv"
  -- this will fail for a list of size < 2 but what the hell
  let wire1 : wire2 : _ = fmap fromText <$> input
  return (wire1, wire2)
