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

import Utils (readProgram)
import IntCode

import BasicPrelude
import Data.List (permutations)

-- | Run sequentially k amplifiers
runAmplifierSequence ::
  Int                 -- ^ the input signal
  -> [Machine] -- ^ the amplifiers programs
  -> [Machine] -- ^ the modified amplifiers programs
runAmplifierSequence _ [] = mempty
runAmplifierSequence inputSignal (p0:programs) = foldl' amplify [p0'] programs
  where
    -- modify the first 'Machine' by adding an extra input and running it
    p0' = runProgram $ p0{inputs = inputs p0 ++ [inputSignal]}
    amplify [] _ = []
    amplify ps p =
      let leftOutputs = outputs . last $ ps
          p'          = p{inputs = inputs p ++ leftOutputs}
      in ps ++ [runProgram p']

runAmplifierSequenceOnce :: Program -> [Int] -> Int
runAmplifierSequenceOnce program phases = last . outputs . last $ programs'
  where
    programs  = freshMachine program Nothing <$> phases
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
  last $ go inputSignal (freshMachine p Nothing <$> phaseSequence)
    where
      go input programs =
        let
          programs'  = runAmplifierSequence input programs
          programs'' = resetMachine <$> programs'
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
