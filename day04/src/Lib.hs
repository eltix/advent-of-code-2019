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

import Preprocessing (readProgram)

import BasicPrelude
import Data.Maybe (listToMaybe)

type Program = [Int]

data OpCode = Add | Multiply | Input | Output
  deriving Show

fromIntToOpCode :: Int -> OpCode
fromIntToOpCode = \case
  1 -> Add
  2 -> Multiply
  3 -> Input
  4 -> Output
  n -> error $ "Unknown opcode: " ++ show n

numberOfParams :: OpCode -> Int
numberOfParams = \case
  Add      -> 3
  Multiply -> 3
  Input    -> 1
  Output   -> 1

-- | This opcode is voluntarily left out the 'OpCode' sum type because we want
-- to directly pattern match against its value instead of passing through
-- 'fromIntToOpCode' which would be inefficient
haltCode :: Int
haltCode = 99

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

data Instruction =
  Instruction
  { opcode     :: OpCode
  , parameters :: [(Mode, Int)]
  } deriving Show

data ReadingState =
    Empty
  | Reading OpCode [Mode] [Int] Int
  -- ^      opcode, modes, params, number of params left to read

splitProgram :: Program -> [Instruction]
splitProgram program = snd . foldl' go (Empty, []) . takeWhile (/= haltCode) $ program
  where
    go (state :: ReadingState, instructions) n = case state of
      Empty ->
        let (opcode, modes) = parseFirstValue n
            state' = Reading opcode modes [] (numberOfParams opcode)
         in (state', instructions)
      Reading opcode modes params 1 ->
        let instruction = Instruction opcode $ zip modes (params ++ [n])
         in (Empty, instructions ++ [instruction])
      Reading opcode modes params count ->
        let state' = Reading opcode modes (params ++ [n]) (count-1)
         in (state', instructions)

data ProgramIO =
  ProgramIO
  { programState :: Program
  , inputs  :: [Int]
  , outputs :: [Int]
  }

runProgram ::
     ProgramIO -- ^ the IntCode program with its inputs
  -> ProgramIO -- ^ the final state of the Intcode program and the outputs
runProgram programIO@(ProgramIO program0 _ _) =
  foldl' runInstructionAndIO programIO instructions
    where
      instructions = splitProgram program0
      runInstructionAndIO p@(ProgramIO program ins outs) instruction =
        case runInstruction program (listToMaybe ins) instruction of
          Pure program'      -> p{programState=program'}
          ReadInput program' -> p{programState=program', inputs=tail ins}
          ReturnsOutput program' output
            -> p{programState=program', outputs = outs ++ [output]}

data InstructionOutput =
    Pure Program
  | ReadInput Program
  | ReturnsOutput Program Int

runInstruction ::
     Program              -- ^ the initial program
  -> Maybe Int            -- ^ maybe an input
  -> Instruction          -- ^ the instruction
  -> InstructionOutput    -- ^ the modified program and maybe an IO action
runInstruction program mbInput (Instruction opcode params) = undefined

part1 :: IO ()
part1 = do
  program <- readInput
  print program

part2 :: IO ()
part2 = undefined

readInput :: IO Program
readInput = readProgram "day04/example1.csv"
