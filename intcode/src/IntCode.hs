{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module IntCode
  ( runProgram
  , Machine(..), Program, State(..)
  , freshMachine, resetMachine
  ) where

import BasicPrelude
import Control.Lens

type Program = [Int]

data OpCode =
  Add | Multiply | Input | Output | JumpIfTrue | JumpIfFalse
  | LessThan | Equals | AdjustBase | Halt
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
  9  -> AdjustBase
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
  AdjustBase  -> 1
  Halt        -> 0

data Mode = Position | Immediate | Relative
  deriving Show

fromIntToMode :: Int -> Mode
fromIntToMode = \case
  0 -> Position
  1 -> Immediate
  2 -> Relative
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

data State = Running | WaitingForInput | Halted
  deriving Show

data Machine =
  Machine
  { pointer :: Int
  , state   :: State
  , base    :: Int
  , inputs  :: [Int]
  , outputs :: [Int]
  , memory  :: [Int]
  }

instance Show Machine where
  show Machine{..} =
    "{ pointer: " ++ show pointer ++ "\n" ++
    ", state: " ++ show state ++ "\n" ++
    ", outputs: " ++ show outputs ++ "\n" ++
    "}"

freshMachine :: Program -> Maybe Int -> Int -> Machine
freshMachine program mbMemSize input =
  Machine
  { pointer = 0
  , state   = Running
  , base    = 0
  , inputs  = [input]
  , outputs = []
  , memory = case mbMemSize of; Nothing -> program; Just m -> take m $ program ++ repeat 0
  }

resetMachine :: Machine -> Machine
resetMachine p = p{state = Running, outputs = []}

runProgram, updateProgram :: Machine -> Machine
runProgram p@Machine{state} = case state of
  Running -> runProgram $ updateProgram p
  _       -> p

updateProgram p@Machine{..} = executeInstruction p opcode $ zip params modes
  where
    (opcode, modes) = parseFirstValue $ memory !! pointer
    numParams       = numberOfParams opcode
    params          = [memory !! (pointer + i) | i <- [1..numParams]]

executeInstruction :: Machine -> OpCode -> [(Int, Mode)] -> Machine
executeInstruction p@Machine{..} opcode ps = programContext'
  where
    p' = p{pointer = pointer + numberOfParams opcode + 1}
    value (param, mode) = case mode of
      Position  -> memory !! param
      Immediate -> param
      Relative  -> memory !! (param + base)
    address (i, mode) = case mode of
      Relative  -> i + base
      _         -> i

    -- programContext' = trace (" pointer= "  ++ show pointer ++ " opcode= " ++ show opcode ++ " params=" ++ show ps ++ " inputs=" ++ show inputs) $ case opcode of
    programContext' = case opcode of
      Add      -> p'{memory = memory & ix (address $ ps !! 2) .~ value (ps !! 0) + value (ps !! 1)}
      Multiply -> p'{memory = memory & ix (address $ ps !! 2) .~ value (ps !! 0) * value (ps !! 1)}
      Input    -> case inputs of
        [] -> p {state = WaitingForInput}
        _  -> p'{memory = memory & ix (address $ ps !! 0) .~ head inputs, inputs = tail inputs}
      Output      -> p'{outputs = outputs ++ [value $ ps !! 0]}
      JumpIfTrue  -> if value (ps !! 0) /= 0 then p{pointer = value (ps !! 1)} else p'
      JumpIfFalse -> if value (ps !! 0) == 0 then p{pointer = value (ps !! 1)} else p'
      LessThan    -> if value (ps !! 0) < value (ps !! 1)
                      then p'{memory = memory & ix (address $ ps !! 2) .~ 1}
                      else p'{memory = memory & ix (address $ ps !! 2) .~ 0}
      Equals      -> if value (ps !! 0) == value (ps !! 1)
                      then p'{memory = memory & ix (address $ ps !! 2) .~ 1}
                      else p'{memory = memory & ix (address $ ps !! 2) .~ 0}
      AdjustBase  -> p'{base = base + value (ps !! 0)}
      Halt        -> p {state = Halted}
