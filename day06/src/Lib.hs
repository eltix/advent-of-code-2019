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
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set

type Object = Text

-- | If we adopt the convention that 'Object' A orbiting around 'Object' B
-- means A is a child of B and B is __the__ parent of A, then
-- this map has the purpose of storing what 'Object' is the parent of each 'Object'
-- The Advent of Code insructions specify that each object has exactly one parent
-- except for COM (center of mass) which has none.
--
--                      child  parent
type OrbitMap = HashMap Object Object

createOrbitMap :: [(Text, Text)] -> OrbitMap
createOrbitMap = foldl' addToMap mempty
  where
    addToMap oMap (parent, child) = HM.insert child parent oMap

-- | Follows the path from an 'Object' to COM. This is useful both for counting
-- the total number of orbits and for finding the shortest path between two
-- 'Objects'.
pathToCOM :: OrbitMap -> Object -> [Object]
pathToCOM oMap obj = case obj `HM.lookup` oMap of
  Just parent -> parent : pathToCOM oMap parent
  Nothing     -> []

part1 :: IO ()
part1 = do
  edges <- readInput
  let
    allObjects :: [Object]
    allObjects = Set.toList . Set.fromList $ fmap fst edges ++ fmap snd edges
    orbitMap  = createOrbitMap edges
    numOrbits = sum . fmap length . fmap (pathToCOM orbitMap) $ allObjects
  putStrLn $  "Total number of orbits = " ++ tshow numOrbits

part2 :: IO ()
part2 = do
  edges <- readInput
  let
    orbitMap = createOrbitMap edges
    youToCom = pathToCOM orbitMap "YOU"
    sanToCom = pathToCOM orbitMap "SAN"
    shortestPath = Set.difference
      (Set.union (Set.fromList youToCom) (Set.fromList sanToCom))
      (Set.intersection (Set.fromList youToCom) (Set.fromList sanToCom))
    -- remove SAN and YOU from the list of 'Object's in the path
    numTransfers = Set.size shortestPath
  print shortestPath
  putStrLn $ "Shortest path from YOU to SAN has " ++ tshow numTransfers ++ " orbit transfers"


readInput :: IO [(Text, Text)]
readInput = do
  let
    toPair = \case
      [a, b] -> (a, b)
      _      -> error "input line does not have expected length of 2"
  content :: [Text] <- T.lines <$> readFile "day06/puzzle_input.csv"
  return $ fmap toPair $ T.splitOn ")" <$> content
