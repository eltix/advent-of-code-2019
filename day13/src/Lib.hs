{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( part1
  , part2
  ) where

import BasicPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import IntCode
import Utils

type Point = (Int, Int)

data TileId = Empty | Wall | Block | Paddle | Ball | Score Int
  deriving (Show, Eq)

instance Enum TileId where
  toEnum 0 = Empty
  toEnum 1 = Wall
  toEnum 2 = Block
  toEnum 3 = Paddle
  toEnum 4 = Ball
  toEnum _ = error "toEnum TileId beyond range"
  fromEnum = undefined

data Tile =
  Tile
  { position :: Point
  , tileId   :: TileId
  } deriving Show

readTile :: [Int] -> Tile
readTile [-1, 0, s] = Tile (-1, 0) $ Score s
readTile [x, y, i]  = Tile (x, y) $ toEnum i
readTile xs         = error $ "Wrong tile format: " ++ show xs

tilesToScreen :: HashMap Point Tile -> IO ()
tilesToScreen tileMap = putStrLn $ T.unlines screenRows <> scoreT
  where
    (allRows, allCols) = ((fst . position) <$> tileMap, (snd . position) <$> tileMap)
    (xa, xb, ya, yb) = (minimum allRows, maximum allRows, minimum allCols, maximum allCols)
    screenRows = [renderRow j | j <- [ya..yb]]
    renderRow j = T.pack [renderTile i j | i <- [xa..xb]]
    renderTile i j = case tileId <$> HM.lookup (i, j) tileMap of
      Just Block  -> '*'
      Just Wall   -> '#'
      Just Ball   -> 'o'
      Just Paddle -> '_'
      _           -> ' '
    scoreT = "SCORE: " ++ case HM.lookup (-1, 0) tileMap of
      Just (Tile _ (Score score)) -> tshow score
      _                           -> "?"

play :: [Int] -> Machine -> Machine
play joystickInputs progCtx = progCtx''
  where
    progCtx'  = progCtx {inputs = joystickInputs, outputs = [], state = Running}
    progCtx'' = runProgram progCtx'

programOutputsToTiles :: [Int] -> HashMap Point Tile
programOutputsToTiles outputs = HM.fromList [(pos, tile) | tile@(Tile pos _) <- tiles]
  where
    tiles = fmap readTile . chunksOf 3 $ outputs

loadGame :: IO Machine
loadGame = do
  program <- readProgram "day13/program.csv"
  let
    program' = freeQuarters 2 program
    progCtx  = freshMachine program' (Just 5000) 0
  return progCtx

part1 :: IO ()
part1 = do
  program <- readProgram "day13/program.csv"
  let
    progCtx   = freshMachine program (Just 10000) 0
    tileMap   = programOutputsToTiles . outputs . runProgram $ progCtx
    numBlocks = HM.size . HM.filter (\t -> case t of; Tile _ Block -> True; _ -> False) $ tileMap
  putStrLn $ "Number of Block tiles = " ++ tshow numBlocks

part2 :: IO ()
part2 = do
  game <- loadGame
  let
    game'     = play [0] game
    baseTiles = programOutputsToTiles . outputs $ game'
  _ <- gameLoop (game', baseTiles)
  return ()

freeQuarters :: Int -> Program -> Program
freeQuarters _ [] = error "Empty program"
freeQuarters quarters (_:restOfProgram) = quarters: restOfProgram

gameLoop :: (Machine, HashMap Point Tile) -> IO (Machine, HashMap Point Tile)
gameLoop (p, tiles) = do
  let
    joystickPos = last [x | (Tile (x, _) Paddle) <- HM.elems tiles]
    ballPos     = last [x | (Tile (x, _) Ball) <- HM.elems tiles]
    joystickInput
      | joystickPos == ballPos = 0
      | joystickPos < ballPos  = 1
      | otherwise              = -1
    p'      = play [joystickInput] p
    tiles'  = programOutputsToTiles . outputs $ p'
    tileMap = HM.union tiles' tiles
  tilesToScreen tileMap
  case [x | (Tile (x, _) Block) <- HM.elems tiles] of
    [] -> return (p', tileMap)
    _  -> gameLoop (p', tileMap)
