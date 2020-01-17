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
  , example
  ) where

import BasicPrelude hiding (Vector)
import qualified Data.Text as T
import Debug.Trace

type Point  = (Int, Int)
type Vector = (Int, Int)

(@->@) :: Point -> Point -> Vector
(xa, ya) @->@ (xb, yb) = (xb - xa, yb - ya)

distance :: Point -> Point -> Int
distance (xa, ya) (xb, yb) = abs (xb - xa) + abs (yb - ya)

colinear :: Vector -> Vector -> Bool
colinear (ux, uy) (vx, vy) = (ux * vy - uy * vx) == 0

dotProduct :: Vector -> Vector -> Int
dotProduct (ux, uy) (vx, vy) = ux * vx + uy * vy

colinearPlus :: Vector -> Vector -> Bool
colinearPlus u v = colinear u v && (dotProduct u v > 0)

norm :: Vector -> Double
norm (ux, uy) = sqrt . fromIntegral $ ux^(2::Int) + uy^(2::Int)

-- | angle between a vector v and the unit vector (0, -1) (vertical pointing upwards)
-- the angle is monotonically increasing in $[0, 2\pi[$
angle :: Vector -> Double
angle u
  | norm u == 0             = error "trying to compute angle of degenerate vector"
  | dotProduct u (1, 0) >=0 = acos $ fromIntegral (dotProduct u (0, -1)) / norm u
  | otherwise               = (-) (2*pi) $ acos $ fromIntegral (dotProduct u (0, -1)) / norm u

visibleAsteroids :: [Point] -> Point -> [Point]
visibleAsteroids [] _ = []
visibleAsteroids (asteroid:asteroids) asteroid0
  | asteroid == asteroid0 = visibleAsteroids asteroids asteroid0
  | otherwise             = (:) asteroid' $ (visibleAsteroids remainingAsteroids asteroid0)
  where
    asteroidsAndVectors          :: [(Point, Vector)]
    asteroidsAndVectors           = [(a, asteroid0 @->@ a) | a <- asteroids]
    vector0                       = asteroid0 @->@ asteroid
    (areColinear, areNotColinear) = partition ((colinearPlus vector0) . snd) asteroidsAndVectors
    colinearAsteroidsAndDistances = trace("areColinear" ++ show areColinear) [(a, distance a asteroid0) | (a, _) <- areColinear]
    asteroid'                     = trace("asteroid' = ") $ traceShowId $ case areColinear of
      [] -> asteroid
      _  -> fst . minimumBy (comparing snd) $ colinearAsteroidsAndDistances
    remainingAsteroids            = fst <$> areNotColinear

bestAsteroid :: [Point] -> (Point, Int)
bestAsteroid asteroids = maximumBy (comparing snd) res
  where res = [(a, length $ visibleAsteroids asteroids a) | a <- asteroids]


part1 :: IO ()
part1 = do
  asteroids <- loadInput
  let (asteroid, numVisibleAsteroids) = bestAsteroid asteroids
  putStrLn $ "Best asteroid: " ++ tshow asteroid ++ " can see "
    ++ tshow numVisibleAsteroids ++ " asteroids."

computeShootingSequence :: [[Point]] -> [Point]
computeShootingSequence clusters = shot ++ (computeShootingSequence clusters')
  where
    (shot, clusters') = foldl' shoot ([], []) clusters
    shoot :: ([Point], [[Point]]) -> [Point] -> ([Point], [[Point]])
    shoot (alreadyShot, remainingAsteroids) [] = (alreadyShot, remainingAsteroids)
    shoot (alreadyShot, remainingAsteroids) (asteroid:asteroids) =
      (alreadyShot++[asteroid], remainingAsteroids ++ [asteroids])

part2 :: IO ()
part2 = do
  asteroids' <- parseRawMap <$> readFile "day10/puzzle_input.csv"
  let
    station = (23, 29) -- found with 'part1'
    -- station = (11, 13)
    asteroids = filter (/= station) asteroids'
  -- let
    -- asteroids = parseRawMap example
    -- station   = (8, 3)
    asteroidsWithAngles = [(a, angle $ station @->@ a) | a <- asteroids]
    asteroidsWithAnglesSorted = sortBy (comparing snd) asteroidsWithAngles
    clusters = fmap (fmap fst) $ groupBy (\a b -> (abs (snd a - snd b) < 1e-14)) asteroidsWithAnglesSorted
    asteroidsSortedByDistance = sortBy (comparing (distance station)) <$> clusters
    shootingSequence = computeShootingSequence asteroidsSortedByDistance
  print $ [shootingSequence !! i | i <- [0,1,2,9,19,49,99,198,199,200,298]]



parseLine :: String -> Int -> [Point]
parseLine s j = foldl' (addAsteroid) [] $ zip s [0..(length s - 1)]
  where
    addAsteroid positions (c, i) = case c of
      '#' -> positions ++ [(i, j)]
      _   -> positions

parseRawMap :: Text -> [Point]
parseRawMap t = concatMap (uncurry parseLine) $ zip lines' [0..(height-1)]
  where
    lines' :: [String] = fmap T.unpack . T.lines $ t
    height = length lines'

loadInput :: IO [Point]
loadInput = parseRawMap <$> readFile "day10/puzzle_input.csv"

example :: Text
example = T.unlines
  [ ".#....#####...#.."
  , "##...##.#####..##"
  , "##...#...#.#####."
  , "..#.....X...###.."
  , "..#.#.....#....##"
  ]
