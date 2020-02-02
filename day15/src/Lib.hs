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
  , interactWithDroid
  ) where

import IntCode hiding (State, state)
import Utils (readProgram)

import qualified System.IO as SIO
import BasicPrelude hiding (Left, Right, Down)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Lens
import Control.Monad.State.Strict

type Point = (Int, Int)

data Direction = Nil | North | South | West | East deriving (Show, Enum)

nextPosition :: Direction -> Point -> Point
nextPosition North (x, y) = (x    , y + 1)
nextPosition South (x, y) = (x    , y - 1)
nextPosition West  (x, y) = (x - 1, y    )
nextPosition East  (x, y) = (x + 1, y    )
nextPosition Nil  _       = undefined

rotateClockWise :: Direction -> Direction
rotateClockWise North = East
rotateClockWise East  = South
rotateClockWise South = West
rotateClockWise West  = North
rotateClockWise _     = undefined

rotateAntiClockWise :: Direction -> Direction
rotateAntiClockWise North = West
rotateAntiClockWise East  = North
rotateAntiClockWise South = East
rotateAntiClockWise West  = South
rotateAntiClockWise _     = undefined

data Tile = Wall | Visited | Droid | Oxygen deriving (Show, Eq)

data Status = Moving | HitWall | FoundOxygen deriving Show

data DroidContext =
  DroidContext
  { _programCtx  :: Machine
  , _environment :: !(HashMap Point Tile)
  , _position    :: Point
  , _direction   :: Direction
  , _status      :: Status
  , _history     :: ![Point]
  } deriving Show

makeLenses ''DroidContext

type WithContext = State DroidContext

move :: WithContext ()
move = do
  ctx <- get
  let
    pos           = ctx ^. position
    nextPos       = nextPosition (ctx ^. direction) pos
    updateEnv     = HM.insert nextPos Droid . HM.insert pos Visited
  modify (over environment updateEnv . set position nextPos . set status Moving . over history (++ [nextPos]))

hitWall :: WithContext ()
hitWall = do
  ctx <- get
  let
    nextPos       = nextPosition (ctx ^. direction) (ctx ^. position)
    updateEnv     = HM.insert nextPos Wall
  modify (over environment updateEnv . set status HitWall)

foundOxygen :: WithContext ()
foundOxygen = do
  ctx <- get
  let
    nextPos       = nextPosition (ctx ^. direction) (ctx ^. position)
    updateEnv     = HM.insert nextPos Oxygen
  modify (over environment updateEnv . set position nextPos . set status FoundOxygen . over history (++ [nextPos]))

execute :: WithContext Int
execute = do
  DroidContext{_programCtx, _direction} <- get
  let
    pCtx'  = (resetMachine _programCtx){inputs = [fromEnum _direction]}
    pCtx'' = runProgram pCtx'
    output = last . outputs $ pCtx''
  modify (set programCtx pCtx'')
  return output

oneStep :: WithContext ()
oneStep = do
  output <- execute
  case output of
    0 -> hitWall
    1 -> move
    2 -> foundOxygen
    _ -> undefined
  return ()

data Strategy = WallOnLeft | WallOnRight

-- | The droid explores the maze on its own. The rule is:
--  "Always have the wall on your left hand side"
-- Surprisingly it works really well :)
-- Note that exploring the maze having the wall on the right hand side is much
-- longer. It really depends on the maze I suppose.
autonomousDroid :: Strategy -> Bool -> DroidContext -> IO DroidContext
autonomousDroid strategy display droidCtx = do
  let
    droidCtx' = snd $ runState oneStep droidCtx
    r         = renderEnvironment $ droidCtx' ^. environment
    updateDir = case droidCtx' ^. status of
      HitWall -> case strategy of WallOnLeft -> rotateClockWise; _ -> rotateAntiClockWise
      _       -> case strategy of WallOnLeft -> rotateAntiClockWise; _ -> rotateClockWise
  when display $ putStrLn r
  case droidCtx' ^. status of
    FoundOxygen -> return droidCtx'
    _           -> autonomousDroid strategy display (droidCtx' & direction %~ updateDir)

-- | You control the droid movements using the usual key strokes:
-- 'z': Up, 'd': Right, 's': Down, 'q': Left
interactWithDroid :: DroidContext -> IO ()
interactWithDroid droidCtx = do
  let
    droidCtx' = snd $ runState oneStep droidCtx
    r         = renderEnvironment $ droidCtx' ^. environment
  putStrLn r
  SIO.hSetBuffering SIO.stdin SIO.NoBuffering
  c <- getChar
  let
    dir = case c of
      'z' -> North
      's' -> South
      'q' -> West
      'd' -> East
      _   -> droidCtx' ^. direction
    droidCtx'' = droidCtx' & direction .~ dir
  interactWithDroid droidCtx''

cutOutBranches :: [Point] -> [Point]
cutOutBranches [] = []
cutOutBranches (x:xs) =
  if x `elem` xs then
    x: (cutOutBranches (tail . dropWhile (/=x) $ xs))
  else
    x : (cutOutBranches xs)

timeTofillWithOxygen :: HashMap Point Tile -> Int
timeTofillWithOxygen area0 = go (area0, Set.singleton oxygenSource) 0
  where
    oxygenSource :: Point
    oxygenSource = head . HM.keys . HM.filter (==Oxygen) $ area0
    go :: (HashMap Point Tile, Set Point) -> Int -> Int
    go (area, oxyFront) i =
      if filledWithO area
        then i
        else go (oneOxygenIteration (area, oxyFront)) (i+1)
    -- stopping criterion: whole area is filled with Oxygen
    filledWithO :: HashMap Point Tile -> Bool
    filledWithO = all id . fmap (\tile -> tile == Wall || tile == Oxygen)
    -- move the Oxygen front one step forward. Fore more efficiency, we keep
    -- track of the front at each iteration so as to not have to recompute it
    oneOxygenIteration :: (HashMap Point Tile, Set Point) -> (HashMap Point Tile, Set Point)
    oneOxygenIteration (area, oxyFront) = (HM.union oxygenatedFront area, freeCellsAdjacentToFront)
      where
        -- Oxygen-free cells that are adjacent to the Oxygen front
        freeCellsAdjacentToFront :: Set Point
        freeCellsAdjacentToFront = Set.fromList . concat $
          adjacentFreeCells <$> (Set.toList oxyFront)

        oxygenatedFront :: HashMap Point Tile
        oxygenatedFront = HM.fromList
          [ (pos, Oxygen)
          | pos <- Set.toList freeCellsAdjacentToFront
          ]

        adjacentFreeCells :: Point -> [Point]
        adjacentFreeCells pos = HM.keys $ HM.filterWithKey
          (\pos' tile -> (not $ tile `elem` [Wall, Oxygen]) && areAdjacent pos pos') area

        areAdjacent :: Point -> Point -> Bool
        areAdjacent (x, y) (x', y') =
          (x' - x, y' - y) `elem` [(0, 1), (0, -1), (1, 0), (-1, 0)]

part1 :: IO ()
part1 = do
  program <- readProgram "day15/program.csv"
  let
    progCtx   = freshMachine program (Just 10000) 0
    droidCtx  = DroidContext progCtx mempty (0, 0) North Moving [(0, 0)]
  finalCtx <- autonomousDroid WallOnLeft True droidCtx
  let path = finalCtx ^. history
  putStrLn $ "Found Oxygen at position " ++ tshow (last path)
  let shortestPath = cutOutBranches path
  putStrLn $ "Shortest path:" ++ tshow shortestPath
  putStrLn $ "Minimal number of moves:" ++ tshow (length shortestPath - 1)

part2 :: IO ()
part2 = do
  program <- readProgram "day15/program.csv"
  let
    progCtx   = freshMachine program (Just 10000) 0
    droidCtx  = DroidContext progCtx mempty (0, 0) North Moving [(0, 0)]
  env1 <- view environment <$> autonomousDroid WallOnLeft False droidCtx
  env2 <- view environment <$> autonomousDroid WallOnRight False droidCtx
  let area = HM.union env1 env2
  putStrLn . renderEnvironment $ area
  let n = timeTofillWithOxygen area
  putStrLn $ "Minutes to fill the area:" ++ tshow n

renderEnvironment :: HashMap Point Tile -> Text
renderEnvironment env = if HM.null env then mempty else T.unlines (reverse renderedRows)
  where
    (rows, cols) = (fmap fst . HM.keys $ env, fmap snd . HM.keys $ env)
    (xa, xb, ya, yb) = (minimum rows, maximum rows, minimum cols, maximum cols)
    renderedRows = [renderRow j | j <- [ya..yb]]
    renderRow j = T.pack [renderLoc i j | i <- [xa..xb]]
    renderLoc 0 0 = '@'
    renderLoc i j = case HM.lookup (i, j) env of
      Just Droid   -> 'D'
      Just Wall    -> '#'
      Just Visited -> '.'
      Just Oxygen  -> 'O'
      _            -> ' '
