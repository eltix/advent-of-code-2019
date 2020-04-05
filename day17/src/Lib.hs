{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( part1
  , part2
  ) where

import IntCode
import Utils (readProgram)

import BasicPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Char
import Control.Lens

data Tile = Scaffold | OpenSpace | Robot Orientation | RobotFell deriving Show

data Orientation = LeftO | RightO | DownO | UpO deriving Show

type Point = (Int, Int)

data Move = Forward | TurnLeft | TurnRight

data NextPositions = NextPositions { _onFront :: Point, _onLeft :: Point, _onRight :: Point}
makeLenses ''NextPositions

nextPositions :: Point -> Orientation -> NextPositions
nextPositions x = (\case
  UpO    -> NextPositions (up'    x) (left'  x) (right' x)
  DownO  -> NextPositions (down'  x) (right' x) (left'  x)
  RightO -> NextPositions (right' x) (up'    x) (down'  x)
  LeftO  -> NextPositions (left'  x) (down'  x) (up'    x)
  )
  where
    up'    = over _2 (subtract 1)
    left'  = over _1 (subtract 1)
    right' = over _1 (+1)
    down'  = over _2 (+1)


tileFromInt :: Int -> Tile
tileFromInt = \case
  35 -> Scaffold
  46 -> OpenSpace
  60 -> Robot LeftO
  62 -> Robot RightO
  86 -> Robot DownO
  94 -> Robot UpO
  88 -> RobotFell
  i  -> error $ "Unknown tile code: " ++ show i

tileToChar :: Tile -> Char
tileToChar = \case
  Scaffold     -> '#'
  OpenSpace    -> '.'
  Robot LeftO  -> '<'
  Robot RightO -> '>'
  Robot DownO  -> 'V'
  Robot UpO    -> '^'
  RobotFell    -> 'X'

isRobot :: Tile -> Bool
isRobot = \case
  Robot _   -> True
  RobotFell -> True
  _         -> False

programOutputToScaffolds :: [Int] -> [[Tile]]
programOutputToScaffolds = foldl' addToScaffolds [[]]
  where
    addToScaffolds scaffolds i =
      let
        (previousLines :: [[Tile]], currentLine :: [Tile]) = (init scaffolds, last scaffolds)
      in case i of
        10 -> scaffolds ++ [[]]
        _  -> previousLines ++ [currentLine ++ [tileFromInt i]]

mapScaffolds :: [[Tile]] -> Set Point
mapScaffolds = Set.unions . fmap (uncurry oneRow) . zip [0..]
  where
    oneRow :: Int -> [Tile] -> Set Point
    oneRow row = Set.fromList . mapMaybe (uncurry $ oneTile row) . zip [0..]
    oneTile :: Int -> Int -> Tile -> Maybe Point
    oneTile row col = \case
      Scaffold -> Just (row, col)
      Robot _  -> Just (row, col)
      _        -> Nothing

locateIntersections :: Set Point -> Set Point
locateIntersections sMap = Set.filter isIntersection $ sMap
  where
    isIntersection p = neighborsOf p `Set.isSubsetOf` sMap
    neighborsOf (x, y) = Set.fromList $
      [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

findRobot :: [[Tile]] -> (Point, Tile)
findRobot scaffolds = ((row, col), robot)
  where
    (row, cols) = head $ filter (any isRobot . snd) $ zip [0..] scaffolds
    (col, robot) = head $ filter (isRobot . snd) $ zip [0..] cols

exploreScaffolds :: Set Point -> (Point, Orientation) -> ([Move], (Point, Orientation))
exploreScaffolds scaffolds = go . ([], )
  where
    go (moves, p) = case nextMove p of
      (Nothing, _)       -> (moves, p)
      (Just newMove, p') -> go (moves ++ [newMove], p')

    nextMove :: (Point, Orientation) -> (Maybe Move, (Point, Orientation))
    nextMove (x, o) = undefined


part1 :: IO ()
part1 = do
  program <- readProgram "day17/program.csv"
  let
    machine = freshMachine program (Just 4000) 0
    scaffolds = programOutputToScaffolds . outputs . runProgram $ machine
  putStrLn . renderScaffolds $ scaffolds
  let
    sMap = mapScaffolds scaffolds
    intersections = locateIntersections sMap
    alignmentParameters = (\(x, y) -> x*y) <$> (Set.toList intersections)
  print $ sum alignmentParameters

-- -- | Ideally this should be automatically computed but I was lazy and
-- -- wrote this manually by looking at the screen rendering of the scaffolding :(
-- robotPath :: [Move]
-- robotPath = [L, F 10] -- , R, F 8, 5, F 8, L, F 10, R, F 8]

part2 :: IO ()
part2 = do
  program <- readProgram "day17/program.csv"
  let
    machine = freshMachine (program & ix 0 .~ 1) (Just 4000) 0
    inputs = [ord 'A', 44, ord 'B', 44, ord 'C', 10, ord 'L', 10, ord '1', 10, ord '1', 10, ord 'y', 10]
    machine' = machine{inputs = [0]}
    res = runProgram $ machine'
  print res
  let
    scaffolds = programOutputToScaffolds . outputs $ res
  putStrLn . renderScaffolds $ scaffolds

renderScaffolds :: [[Tile]] -> Text
renderScaffolds = T.unlines . fmap renderRow
  where
    renderRow = T.pack . fmap tileToChar
