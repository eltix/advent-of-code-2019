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

import IntCode hiding (State)
import Utils (readProgram)

import BasicPrelude
-- import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Set as Set
-- import Data.Char
import Control.Lens hiding (left', right')
import Control.Monad.State.Strict

data Tile = Scaffold | OpenSpace | Robot Orientation | RobotFell deriving Show

data Orientation = LeftO | RightO | DownO | UpO deriving Show

type Point = (Int, Int)

data Move = Forward | TurnLeft | TurnRight

instance Show Move where
  show Forward = "F"
  show TurnLeft = "L"
  show TurnRight = "R"

turnLeft, turnRight :: Orientation -> Orientation
turnLeft LeftO   = DownO
turnLeft DownO   = RightO
turnLeft RightO  = UpO
turnLeft UpO     = LeftO
turnRight LeftO  = UpO
turnRight UpO    = RightO
turnRight RightO = DownO
turnRight DownO  = LeftO

moveForward :: Orientation -> Point -> Point
moveForward orient = onFront . flip nextPositions orient

movesToChars :: [Move] -> [Char]
movesToChars moves = if counterF == 0 then charsF else charsF ++ [head . show $ counterF]
  where
    (charsF, counterF::Int) = foldl' parse ([], 0::Int) moves
    parse (chars, counter) = \case
      TurnLeft ->
        let chars' = if counter == 0 then ['L'] else show counter ++ ['L']
        in (chars ++ chars', 0)
      TurnRight ->
        let chars' = if counter == 0 then ['R'] else show counter ++ ['R']
        in (chars ++ chars', 0)
      Forward -> (chars, counter + 1)



data NextPositions = NextPositions { onFront :: Point, onLeft :: Point, onRight :: Point}
  deriving Show

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

orientationFromRobot :: Tile -> Orientation
orientationFromRobot (Robot o) = o
orientationFromRobot t = error $ "Tile is not a robot: " ++ show t

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
      Scaffold -> Just (col, row)
      Robot _  -> Just (col, row)
      _        -> Nothing

locateIntersections :: Set Point -> Set Point
locateIntersections sMap = Set.filter isIntersection $ sMap
  where
    isIntersection p = neighborsOf p `Set.isSubsetOf` sMap
    neighborsOf (x, y) = Set.fromList $
      [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

findRobot :: [[Tile]] -> (Point, Orientation)
findRobot scaffolds = ((col, row), orientationFromRobot robot)
  where
    (row, cols) = head $ filter (any isRobot . snd) $ zip [0..] scaffolds
    (col, robot) = head $ filter (isRobot . snd) $ zip [0..] cols


data RobotState =
  RobotState
  { _position    :: Point
  , _orientation :: Orientation
  , _moves       :: [Move]
  , _scaffolding :: Set Point
  , _isStuck     :: Bool
  } deriving Show
makeLenses ''RobotState
type RobotM = State RobotState


exploreScaffolding :: RobotState -> RobotState
exploreScaffolding s =
  case snd $ runState moveOnce s of
    s'@(RobotState _ _ _ _ True) -> s'
    s'                           -> exploreScaffolding s'


moveOnce :: RobotM ()
moveOnce = do
  scaff <- gets (view scaffolding)
  positions <- gets (uncurry nextPositions . (view position &&& view orientation))
  case nextMove scaff positions of
    Just move -> do
      robotTurns move
      modify' (over moves (++ [move]))
    Nothing -> modify' (set isStuck True)

robotTurns :: Move -> RobotM ()
robotTurns = \case
  TurnLeft  -> modify' (over orientation turnLeft)
  TurnRight -> modify' (over orientation turnRight)
  Forward   -> do
    orientation' <- gets (view orientation)
    modify' (over position (moveForward orientation'))


-- | This works based on the assumption that there is no "T-type" intersection
-- (robot can't move forward but can turn either left or right) in the maze.
nextMove :: Set Point -> NextPositions -> Maybe Move
nextMove sMap (NextPositions front left' right')
  | front  `Set.member` sMap = Just Forward
  | left'  `Set.member` sMap = Just TurnLeft
  | right' `Set.member` sMap = Just TurnRight
  | otherwise                = Nothing


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

robotMoves :: IO [Move]
robotMoves = do
  program <- readProgram "day17/program.csv"
  let
    machine = freshMachine program (Just 4000) 0
    scaffolds = programOutputToScaffolds . outputs . runProgram $ machine
  putStrLn . renderScaffolds $ scaffolds
  let
    (pos0, orientation0) = findRobot scaffolds
    sMap = mapScaffolds scaffolds
  print (pos0, orientation0)
  print $ nextPositions pos0 orientation0
  let
    state0 = RobotState pos0 orientation0 [] sMap False
    res = exploreScaffolding state0
  return $ res ^. moves

part2 :: IO ()
part2 = do
  moves' <- robotMoves
  let
    s = movesToChars moves'
    a = "L10R8R8"
    b = "R10L12R10"
    c = "L10L12R8R10R10L12R10"
  print $ a ++ a ++ c ++ c ++ c ++ b ++ a == s

renderScaffolds :: [[Tile]] -> Text
renderScaffolds = T.unlines . fmap renderRow
  where
    renderRow = T.pack . fmap tileToChar
