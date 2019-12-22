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
import Data.List (permutations)

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

data State = Running | WaitingForInput | Halted
  deriving Show

data ProgramContext =
  ProgramContext
  { program :: Program
  , pointer :: Int
  , state   :: State
  , inputs  :: [Int]
  , outputs :: [Int]
  } deriving Show

freshProgramContext :: Program -> Int -> ProgramContext
freshProgramContext program input = ProgramContext program 0 Running [input] []

resetProgramContext :: ProgramContext -> ProgramContext
resetProgramContext p = p{state = Running, outputs = []}

runProgram, updateProgram :: ProgramContext -> ProgramContext
runProgram p@ProgramContext{state} = case state of
  Running -> runProgram $ updateProgram p
  _       -> p

updateProgram p@ProgramContext{..} = executeInstruction p opcode $ zip params modes
  where
    (opcode, modes) = parseFirstValue $ program !! pointer
    numParams       = numberOfParams opcode
    params          = [program !! (pointer + i) | i <- [1..numParams]]

executeInstruction :: ProgramContext -> OpCode -> [(Int, Mode)] -> ProgramContext
executeInstruction p@ProgramContext{..} opcode ps = programContext'
  where
    p' = p{pointer = pointer + numberOfParams opcode + 1}
    value (param, mode) = case mode of
      Position  -> program !! param
      Immediate -> param

    programContext' = case opcode of
      Add      -> p'{program = program & ix (fst $ ps !! 2) .~ value (ps !! 0) + value (ps !! 1)}
      Multiply -> p'{program = program & ix (fst $ ps !! 2) .~ value (ps !! 0) * value (ps !! 1)}
      Input    -> case inputs of
        [] -> p {state = WaitingForInput}
        _  -> p'{program = program & ix (fst $ ps !! 0) .~ head inputs, inputs = tail inputs}
      Output   -> p'{outputs = outputs ++ [value $ ps !! 0]}
      JumpIfTrue  -> if value (ps !! 0) /= 0 then p{pointer = value (ps !! 1)} else p'
      JumpIfFalse -> if value (ps !! 0) == 0 then p{pointer = value (ps !! 1)} else p'
      LessThan    -> if value (ps !! 0) < value (ps !! 1)
                      then p'{program = program & ix (fst $ ps !! 2) .~ 1}
                      else p'{program = program & ix (fst $ ps !! 2) .~ 0}
      Equals      -> if value (ps !! 0) == value (ps !! 1)
                      then p'{program = program & ix (fst $ ps !! 2) .~ 1}
                      else p'{program = program & ix (fst $ ps !! 2) .~ 0}

      Halt     -> p {state = Halted} -- Halt returns the program unchanged

-- | Run sequentially k amplifiers
runAmplifierSequence ::
  Int                 -- ^ the input signal
  -> [ProgramContext] -- ^ the amplifiers programs
  -> [ProgramContext] -- ^ the modified amplifiers programs
runAmplifierSequence _ [] = mempty
runAmplifierSequence inputSignal (p0:programs) = foldl' amplify [p0'] programs
  where
    -- modify the first 'ProgramContext' by adding an extra input and running it
    p0' = runProgram $ p0{inputs = inputs p0 ++ [inputSignal]}
    amplify [] _ = []
    amplify ps p =
      let leftOutputs = outputs . last $ ps
          p'          = p{inputs = inputs p ++ leftOutputs}
      in ps ++ [runProgram p']

runAmplifierSequenceOnce :: Program -> [Int] -> Int
runAmplifierSequenceOnce program phases = last . outputs . last $ programs'
  where
    programs  = freshProgramContext program <$> phases
    programs' = runAmplifierSequence 0 programs

findHighestOutputSignal :: Program -> (Int, [Int])
findHighestOutputSignal p = (highestSignal, phaseSequence)
  where
    allPhaseSequences = permutations [0..4]
    signals = runAmplifierSequenceOnce p <$> allPhaseSequences
    (highestSignal, phaseSequence) =
      maximumBy (comparing fst) $ zip signals allPhaseSequences

feedBackLoop ::
  Program  -- ^ the program
  -> Int   -- ^ the input signal
  -> [Int] -- ^ the phase sequence
  -> Int   -- ^ the output signal of last loop
feedBackLoop p inputSignal phaseSequence =
  last $ go inputSignal (freshProgramContext p <$> phaseSequence)
    where
      go input programs =
        let
          programs'  = runAmplifierSequence input programs
          programs'' = resetProgramContext <$> programs'
        in case outputs . last $ programs' of
          []       -> []
          output:_ -> (:) output $ go output programs''

findHighestLoopSignal :: Program -> (Int, [Int])
findHighestLoopSignal p = (highestSignal, phaseSequence)
  where
    allPhaseSequences = permutations [5..9]
    signals = feedBackLoop p 0 <$> allPhaseSequences
    (highestSignal, phaseSequence) =
      maximumBy (comparing fst) $ zip signals allPhaseSequences

part1 :: IO ()
part1 = do
  let
    [p1, p2, p3] = testPrograms
    r1 = runAmplifierSequenceOnce p1 [4,3,2,1,0]
    r2 = runAmplifierSequenceOnce p2 [0,1,2,3,4]
    r3 = runAmplifierSequenceOnce p3 [1,0,4,3,2]
  print (r1, r2, r3)
  program <- readProgram "day07/program.csv"
  let (highestSignal, phaseSequence) = findHighestOutputSignal program
  putStrLn $ "Highest signal = " ++ tshow highestSignal
  putStrLn $ "Phase sequence = " ++ tshow phaseSequence

part2 :: IO ()
part2 = do
  example1 <- readProgram "day07/example1.csv"
  example2 <- readProgram "day07/example2.csv"
  putStrLn $ "Example 1: " ++ (tshow $ feedBackLoop example1 0 [9,8,7,6,5])
  putStrLn $ "Example 2: " ++ (tshow $ feedBackLoop example2 0 [9,7,8,5,6])
  program <- readProgram "day07/program.csv"
  let (highestSignal, phaseSequence) = findHighestLoopSignal program
  putStrLn $ "Highest signal = " ++ tshow highestSignal
  putStrLn $ "Phase sequence = " ++ tshow phaseSequence


testPrograms :: [Program]
testPrograms = [p1, p2, p3]
  where
    p1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
    p2 = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
    p3 = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,
          31,1,32,31,31,4,31,99,0,0,0]
