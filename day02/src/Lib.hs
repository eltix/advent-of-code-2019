{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Lib
  ( part1
  , part2
  ) where

import Utils (readProgram)
import IntCode

import BasicPrelude
import Control.Lens

findNounAndVerb ::
  Program             -- ^ program
  -> Int              -- ^ target value
  -> Maybe (Int, Int) -- ^ noun and verb
findNounAndVerb prog target = fmap fst . find ((== target) . snd) $ zip candidates outputs'
  where
    candidates = [(noun, verb) | noun <- [0..99] , verb <- [0..99]]
    programs   = updateProgram <$> candidates
    outputs'   = head . memory . runProgram . (\p -> freshProgramContext p Nothing 0) <$> programs
    updateProgram (noun, verb) = prog & ix 1 .~ noun & ix 2 .~ verb

part1 :: IO ()
part1 = do
  p :: [Int] <- readProgram "day02/part1.csv"
  let
    p'  = p & ix 1 .~ 12 & ix 2 .~ 2
    pCtx = runProgram $ freshProgramContext p' Nothing 0
  putStrLn $ "final state at position 0 = " ++ tshow (head . memory $ pCtx)

part2 :: IO ()
part2 = do
  program :: [Int] <- readProgram "day02/part1.csv"
  let
    target                 = 19690720
    mbNoundAndVerb         = findNounAndVerb program target
    transform (noun, verb) = 100 * noun + verb
  print mbNoundAndVerb
  putStrLn $ "100 * noun + verb = " ++ tshow (transform <$> mbNoundAndVerb)
