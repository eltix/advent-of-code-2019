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

part1 :: IO ()
part1 = do
  program <- readInput
  putStrLn "Please enter input:"
  input <- read <$> getLine
  let programCtx = freshProgramContext program Nothing input
      result     = last . outputs . runProgram $ programCtx
  print result

part2 :: IO ()
part2 = do
  program <- readInput
  putStrLn "Please enter input:"
  input <- read <$> getLine
  let programCtx = freshProgramContext program Nothing input
      result     = last . outputs . runProgram $ programCtx
  print result

testPrograms :: [Program]
testPrograms = [equals8P, lessThan8P, equals8I, lessThan8I]
  where
    equals8P   = [3,9,8,9,10,9,4,9,99,-1,8]
    lessThan8P = [3,9,7,9,10,9,4,9,99,-1,8]
    equals8I   = [3,3,1108,-1,8,3,4,3,99]
    lessThan8I = [3,3,1107,-1,8,3,4,3,99]

readInput :: IO Program
readInput = readProgram "day05/program.csv"
