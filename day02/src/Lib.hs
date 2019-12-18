{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Lib
  ( part1
  , part2
  , runProgram
  , findNounAndVerb
  ) where

import BasicPrelude
import qualified Data.Text as T
import Control.Lens


runProgram ::
  [Int]    -- ^ the Intcode program
  -> [Int] -- ^ final state
runProgram program = fst $ foldl' processOpCode (program, True) [0,4..lastPos]
  where
    lastPos = 4 * ((length program -1)`div` 4)
    processOpCode :: ([Int], Bool) -> Int -> ([Int], Bool)
    processOpCode (state, False) _ = (state, False)
    processOpCode (state, True ) i = (newState, continue)
      where
        op           = state !! i
        (posA, posB) = (state !! (i+1), state !! (i+2))
        (a, b)       = (state !! posA, state !! posB)
        posC         = state !! (i+3)
        (newState, continue) = case applyOpCode op a b of
          Just c  -> (state & ix posC .~ c, True )
          Nothing -> (state               , False)

applyOpCode :: Int -> Int -> Int -> Maybe Int
applyOpCode op a b = case op of
  1  -> Just $ a + b
  2  -> Just $ a * b
  99 -> Nothing
  i  -> error $ "Unknown opcode: " ++ show i

findNounAndVerb ::
  [Int]               -- ^ program
  -> Int              -- ^ target value
  -> Maybe (Int, Int) -- ^ noun and verb
findNounAndVerb program target = fmap fst . find ((== target) . snd) $ zip candidates outputs
  where
    candidates = [(noun, verb) | noun <- [0..99] , verb <- [0..99]]
    programs   = updateProgram <$> candidates
    outputs    = head . runProgram <$> programs
    updateProgram (noun, verb) = program & ix 1 .~ noun & ix 2 .~ verb

part1 :: IO ()
part1 = do
  program :: [Int] <- readProgram
  let
    program'   = program & ix 1 .~ 12 & ix 2 .~ 2
    finalState = runProgram program'
  putStrLn $ "final state at position 0 = " ++ tshow (head finalState)

part2 :: IO ()
part2 = do
  program :: [Int] <- readProgram
  let
    target                 = 19690720
    mbNoundAndVerb         = findNounAndVerb program target
    transform (noun, verb) = 100 * noun + verb
  print mbNoundAndVerb
  putStrLn $ "100 * noun + verb = " ++ tshow (transform <$> mbNoundAndVerb)

readProgram :: IO [Int]
readProgram = fmap read . T.splitOn "," . head . T.lines <$> readFile "day02/part1.csv"
