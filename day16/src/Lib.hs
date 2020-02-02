{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

module Lib
  ( part1
  , part2
  ) where

import BasicPrelude

import Utils (loadIntList, stringToIntList)

import qualified Data.Text as T
import Control.DeepSeq (($!!))

type Signal = [Int]

basePattern :: [Int]
basePattern = [0, 1, 0, -1]

-- | Beware: this function returns an infinite list. The output should be zipped with
-- a finite list to avoid any infinite loop
expandPattern :: Int -> [Int] -> [Int]
expandPattern replication =
  tail . concat . repeat . concat . fmap (replicate replication)

fftPhase :: Signal -> Signal
fftPhase signal = (takeLastDigit . flip applyPattern signal) <$> [1..(length signal)]

applyPattern :: Int -> Signal -> Int
applyPattern i signal = sum $ zipWith (*) signal (expandPattern i basePattern)

takeLastDigit :: Int -> Int
takeLastDigit = abs . (`rem` 10)

-- | More efficient FFT where we take advantage of the fact that:
--
--    * the input signal is repeated
--    * the pattern matrix is upper triangular
--    * the message offset is in the second half of the output signal
backwardFft :: Int -> Int -> Signal -> Text
backwardFft signalRepetition numPhases signal = message
  where
    n            = length signal
    -- message offset: where to start reading the first 8 digits forming the message
    offset       = sum [ (i * 10^p) | (p, i) <- zip [6::Int,5..0] $ take 7 signal]
    -- how many times is the signal repeated before the message offset
    k            = div offset n
    -- where in the last signal chunk is the message starting
    localOffset  = rem offset n
    -- only consider the _end_ of the signal. Due to the upper triangular
    -- structure of the pattern matrix we need not consider the rest
    signals0     = replicate (signalRepetition - k) signal
    -- Apply the transformation @numPhases@ times
    signals      = foldl' f signals0 [1..numPhases]
    -- ($!!) forces deep evaluation of @signals'@ to prevent memory bloat
    -- I haven't investigated this furter
    f signals' _ = oneBackwardFftPhase $!! signals'
    -- format the end message
    message      = pretty . drop localOffset . head $ signals

oneBackwardFftPhase :: [Signal] -> [Signal]
oneBackwardFftPhase signals = res
  where
    -- signals resulting from multiplying each input chunk with the diagonal block of
    -- the pattern matrix, i.e. an upper triangular matrix of all ones
    diagBlocks = upperOnesMult <$> signals
    -- each chunk multiplied by a matrix of all ones (the upper triangular blocks
    -- of the pattern matrix)
    sums       = sum <$> signals
    -- for a given chunk, multiply all the lower (or subsequent) chunks
    -- with the blocks of all ones
    upperDiags = (scanr1 (+) $ tail sums) ++ [0]
    -- each chunk transformation consists in multiplying it with the diagonal block
    -- and then adding the multiplications of the lower (or subsequent) chunks
    -- with the blocks of all ones
    res        = zipWith (\c -> fmap takeLastDigit . fmap (+c)) upperDiags diagBlocks

-- | An upper triangular matrix of ones (n, n) multiplied by a signal of size n
upperOnesMult :: Signal -> Signal
upperOnesMult = scanr1 (+)

part1 :: IO ()
part1 = do
  let signal' = take 4 $ iterate' fftPhase ex1
  putStrLn $ "Small example after 4 phases:" ++ T.intercalate ", " (pretty <$> signal')
  putStrLn $ "The other examples after 100 phases:"
  putStrLn . T.unlines $ (pretty . (!!100) . iterate' fftPhase) <$> [ex2, ex3, ex4]
  signal <- loadIntList "day16/puzzle_input.csv"
  putStrLn $ "Puzzle solution (first 8 digits): " ++ (pretty . (!!100) . iterate' fftPhase $ signal)

part2 :: IO ()
part2 = do
  let [res1, res2, res3] = backwardFft 10000 100 <$> [ex5, ex6, ex7]
  putStrLn $ "Example 1: " <> res1
  putStrLn $ "Example 2: " <> res2
  putStrLn $ "Example 3: " <> res3
  signal <- loadIntList "day16/puzzle_input.csv"
  let sol = backwardFft 10000 100 signal
  putStrLn $ "Puzzle solution (first 8 digits): " <> sol

pretty :: Signal -> Text
pretty = T.concat . take 8 . fmap tshow

ex1, ex2, ex3, ex4, ex5, ex6, ex7 :: Signal
ex1 = [1, 2, 3, 4, 5, 6, 7, 8]
ex2 = stringToIntList "80871224585914546619083218645595"
ex3 = stringToIntList "19617804207202209144916044189917"
ex4 = stringToIntList "69317163492948606335995924319873"
ex5 = stringToIntList "03036732577212944063491565474664"
ex6 = stringToIntList "02935109699940807407585447034323"
ex7 = stringToIntList "03081770884921959731165446850517"
