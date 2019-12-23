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

import Preprocessing (readProgram)
import IntCode

import BasicPrelude


part1 :: IO ()
part1 = do
  program <- readProgram "day09/program.csv"
  let
    progCtxs  = (\p -> freshProgramContext p (Just 1000) 0) <$> examplePrograms
    progCtxs' = runProgram <$> progCtxs
  forM_ progCtxs' (putStrLn . tshow . outputs)
  putStrLn . tshow . outputs . runProgram $ freshProgramContext program (Just 10000) 1

part2 :: IO ()
part2 = do
  program <- readProgram "day09/program.csv"
  putStrLn . tshow . outputs . runProgram $ freshProgramContext program (Just 10000) 2

examplePrograms :: [Program]
examplePrograms = [p1, p2, p3]
  where
    p1 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
    p2 = [1102,34915192,34915192,7,4,7,99,0]
    p3 = [104,1125899906842624,99]
