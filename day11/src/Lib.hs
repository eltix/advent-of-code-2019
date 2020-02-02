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

import IntCode
import Utils (readProgram)

import BasicPrelude hiding (Left, Right, Down)
import Data.Bifunctor (bimap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
-- import Debug.Trace

type Point = (Int, Int)

data Facing = Up | Right | Down | Left deriving Show

data Color = Black | White deriving Show

colorToInt :: Color -> Int
colorToInt Black = 0
colorToInt White = 1

intToColor :: Int -> Color
intToColor 0 = Black
intToColor 1 = White
intToColor i = error $ "Unknown color code: " ++ show i

data Rotation = Trigo | Clock deriving Show

intToRotation :: Int -> Rotation
intToRotation 0 = Trigo
intToRotation 1 = Clock
intToRotation i = error $ "Unknown rotation code: " ++ show i

data RobotContext =
  RobotContext
  { programCtx :: Machine
  , painted    :: HashMap Point Color
  , position   :: Point
  , facing     :: Facing
  } deriving Show

move :: Rotation -> (Point, Facing) -> (Point, Facing)
move rotation = forward . rotate rotation

rotate :: Rotation -> (Point, Facing) -> (Point, Facing)
rotate Trigo (p, Up)    = (p, Left)
rotate Trigo (p, Right) = (p, Up)
rotate Trigo (p, Down)  = (p, Right)
rotate Trigo (p, Left)  = (p, Down)
rotate Clock (p, Up)    = (p, Right)
rotate Clock (p, Right) = (p, Down)
rotate Clock (p, Down)  = (p, Left)
rotate Clock (p, Left)  = (p, Up)

forward :: (Point, Facing) -> (Point, Facing)
forward ((x, y), f@Up)    = ((x,   y+1), f)
forward ((x, y), f@Right) = ((x+1, y  ), f)
forward ((x, y), f@Down)  = ((x,   y-1), f)
forward ((x, y), f@Left)  = ((x-1, y  ), f)

runRobotProgram :: Color -> RobotContext -> RobotContext
runRobotProgram hullColor r =
  let r'       = runOneRobotStep hullColor r
      progCtx' = programCtx r'
  in
  case state progCtx' of
    Halted -> r'
    _      -> runRobotProgram hullColor r'{programCtx = resetMachine progCtx'}

runOneRobotStep :: Color -> RobotContext -> RobotContext
runOneRobotStep hullColor RobotContext{..} =
  RobotContext programCtx'' (HM.insert position colorToPaint painted) position' facing'
  where
    panelColor = case HM.lookup position painted of
      Just color -> color
      Nothing    -> hullColor
    programCtx' = programCtx {inputs = [colorToInt panelColor]}
    programCtx'' = runProgram programCtx'
    (colorToPaint, rotation) =
      bimap intToColor intToRotation
      ((outputs programCtx'') !! 0, (outputs programCtx'') !! 1)
    (position', facing') = move rotation (position, facing)

paintHull ::
  Color                  -- ^ initial color of the hull
  -> Program
  -> HashMap Point Color
paintHull hullColor program = painted $ robotCtx'
  where
    progCtx       = freshMachine program (Just 10000) 0
    robotCtx      = RobotContext progCtx HM.empty (0,0) Up
    robotCtx'     = runRobotProgram hullColor robotCtx

flipUpsideDown :: HashMap Point Color -> HashMap Point Color
flipUpsideDown = HM.fromList . fmap (\((x, y), c) -> ((x, -y), c)). HM.toList

renderPaint :: HashMap Point Color -> Text
renderPaint painting = T.unlines paintRows
  where
    (rows, cols) = (fmap fst . HM.keys $ painting, fmap snd . HM.keys $ painting)
    (xa, xb, ya, yb) = (minimum rows, maximum rows, minimum cols, maximum cols)
    paintRows = [renderRow j | j <- [ya..yb]]
    renderRow j = T.pack [renderPanel i j | i <- [xa..xb]]
    renderPanel i j = case HM.lookup (i, j) painting of
      Just White -> '#'
      _          -> ' '

part1 :: IO ()
part1 = do
  program <- readProgram "day11/program.csv"
  let paintedPanels = HM.size . paintHull Black $ program
  putStrLn . tshow $ paintedPanels

part2 :: IO ()
part2 = do
  program <- readProgram "day11/program.csv"
  let
    painting = paintHull White program
    painting' = flipUpsideDown painting
  putStrLn . renderPaint $ painting'
