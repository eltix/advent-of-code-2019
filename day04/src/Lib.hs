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
  , testPrograms
  ) where

import Preprocessing (readProgram)

import BasicPrelude
import Control.Lens

type Program = [Int]

data OpCode =
  Add | Multiply | Input | Output | JumpIfTrue | JumpIfFalse
  | LessThan | Equals | Halt
    deriving Show

fromIntToOpCode :: Int -> OpCode
fromIntToOpCode = \case
  1  -> Add
  2  -> Multiply
  3  -> Input
  4  -> Output
  5  -> JumpIfTrue
  6  -> JumpIfFalse
  7  -> LessThan
  8  -> Equals
  99 -> Halt
  n  -> error $ "Unknown opcode: " ++ show n

numberOfParams :: OpCode -> Int
numberOfParams = \case
  Add         -> 3
  Multiply    -> 3
  Input       -> 1
  Output      -> 1
  JumpIfTrue  -> 2
  JumpIfFalse -> 2
  LessThan    -> 3
  Equals      -> 3
  Halt        -> 0

data Mode = Position | Immediate
  deriving Show

fromIntToMode :: Int -> Mode
fromIntToMode = \case
  0 -> Position
  1 -> Immediate
  m -> error $ "Unknown parameter mode: " ++ show m

parseFirstValue :: Int -> (OpCode, [Mode])
parseFirstValue value = (opCode, fromIntToMode <$> [m1, m2, m3])
  where
    opCode   = fromIntToOpCode $ mod value 100 -- returns the right-most 2 digits of the value
    h1       = div value 100 -- the left-most remaining digits
    -- probably worth factoring this out somehow but hey
    (m1, h2) = digits h1
    (m2, h3) = digits h2
    (m3, _)  = digits h3
    -- returns the right-most digit of an integer and the remaining digits
    digits n = (mod n 10, div n 10)

data ProgramIO =
  ProgramIO
  { program :: Program
  , pointer :: Int
  , input   :: Int
  , outputs :: [Int]
  } deriving Show

runProgram, updateProgram :: ProgramIO -> ProgramIO
runProgram p@ProgramIO{pointer}
  | pointer < 0 = p
  | otherwise   = runProgram $ updateProgram p

updateProgram p@ProgramIO{..} = executeInstruction p opcode $ zip params modes
  where
    (opcode, modes) = parseFirstValue $ program !! pointer
    numParams       = numberOfParams opcode
    params          = [program !! (pointer + i) | i <- [1..numParams]]

executeInstruction :: ProgramIO -> OpCode -> [(Int, Mode)] -> ProgramIO
executeInstruction p@ProgramIO{..} opcode ps = programIO'
  where
    p' = p{pointer = pointer + numberOfParams opcode + 1}
    value (param, mode) = case mode of
      Position  -> program !! param
      Immediate -> param

    programIO' = case opcode of
      Add      -> p'{program = program & ix (fst $ ps !! 2) .~ value (ps !! 0) + value (ps !! 1)}
      Multiply -> p'{program = program & ix (fst $ ps !! 2) .~ value (ps !! 0) * value (ps !! 1)}
      Input    -> p'{program = program & ix (fst $ ps !! 0) .~ input}
      Output   -> p'{outputs = outputs ++ [value $ ps !! 0]}
      JumpIfTrue  -> if value (ps !! 0) /= 0 then p{pointer = value (ps !! 1)} else p'
      JumpIfFalse -> if value (ps !! 0) == 0 then p{pointer = value (ps !! 1)} else p'
      LessThan    -> if value (ps !! 0) < value (ps !! 1)
                      then p'{program = program & ix (fst $ ps !! 2) .~ 1}
                      else p'{program = program & ix (fst $ ps !! 2) .~ 0}
      Equals      -> if value (ps !! 0) == value (ps !! 1)
                      then p'{program = program & ix (fst $ ps !! 2) .~ 1}
                      else p'{program = program & ix (fst $ ps !! 2) .~ 0}

      Halt     -> p {pointer = -1} -- Halt returns the program unchanged

part1 :: IO ()
part1 = do
  program <- readInput
  let
    programIO = ProgramIO program 0 1 []
    result    = runProgram programIO
  print result

part2 :: IO ()
part2 = do
  program <- readInput
  putStrLn "Please enter input:"
  input <- read <$> getLine
  -- let
  --   samplePrograms = (\p -> ProgramIO p 0 input []) <$> testPrograms
  --   results    = runProgram <$> samplePrograms
  let result = runProgram $ ProgramIO program 0 input []

  print result

testPrograms :: [Program]
testPrograms = [equals8P, lessThan8P, equals8I, lessThan8I]
  where
    equals8P   = [3,9,8,9,10,9,4,9,99,-1,8]
    lessThan8P = [3,9,7,9,10,9,4,9,99,-1,8]
    equals8I   = [3,3,1108,-1,8,3,4,3,99]
    lessThan8I = [3,3,1107,-1,8,3,4,3,99]

readInput :: IO Program
readInput = readProgram "day04/example2.csv"
