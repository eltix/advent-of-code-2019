{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Lib
  ( part1
  , part2
  ) where

import BasicPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Monad.State

type Compound = Text
-- A convenient alias since a 'Compound' is always associated with its quantity
type Reactive = (Int, Compound)
-- A rea
type ReactionMap = HashMap Compound (Int, [Reactive])
-- A pot is where the reactions take place. When too many chemicals are produced
-- by a reactions, the leftovers are kept in the 'Pot'.
type Pot = HashMap Compound Int

-- | How many items of a 'Compound' are in the 'Pot'
pop :: Compound -> State Pot Int
pop compound = state $ \leftovers -> case HM.lookup compound leftovers of
  Nothing -> (0, leftovers)
  Just n  -> (n, HM.insert compound 0 leftovers)

-- | Add a @n@ items of a 'Compound' to the 'Pot'
push :: Reactive -> State Pot ()
push (n, compound) = state $ \leftovers -> ((), HM.insert compound n leftovers)

-- | How many "ORE" are needed to produced a certain amount @n@ of "FUEL"
oreNeeded :: ReactionMap -> Reactive -> Int
oreNeeded reacMap reactive = fst . runState (runReactions reacMap reactive) $ mempty

-- | Recursively run reactions to produced the required quantity of "FUEL"
-- This runs in the 'State Pot' Monad to keep track of the leftover chemicals
-- so they can be reused in other reactions.
runReactions :: ReactionMap -> Reactive -> State Pot Int
runReactions reacMap reactive@(n, _) = do
  mbReactives <- runOneReaction reacMap reactive
  case mbReactives of
    Nothing -> pure n
    Just reactives  -> fmap sum . mapM (runReactions reacMap) $ reactives

-- | Returns the list of required 'Reactive's needed
-- to generate a given 'Reactive'. Runs in the 'State Pot' Monad so that it
-- can consume and put leftover chemicals.
runOneReaction :: ReactionMap -> Reactive -> State Pot (Maybe [Reactive])
runOneReaction _ (_, "ORE")         = pure Nothing
runOneReaction reacMap (needed, compound) = do
  available <- pop compound
  let actuallyNeeded = needed - available
  if actuallyNeeded <= 0 then do
    push (available - needed, compound) >> pure (Just [])
  else do
    let (produced, reactives) = reactivesNeeded reacMap (actuallyNeeded, compound)
    push (produced - actuallyNeeded, compound)
    pure . Just $ reactives

-- | What 'Reactive's are needed to produce a certain quantity @n@ of a 'Compound' @c@.
-- Also returns the actual quantity @p@ of 'Compound's produced. Indeed, @p@ can be
-- greater than @c@. We need @p@ to track leftover compounds in the 'Pot'.
reactivesNeeded :: ReactionMap -> Reactive -> (Int, [Reactive])
reactivesNeeded reacMap (needed, compound) = (produced*k, (first (*k)) <$> reactives)
  where
    (produced, reactives) = reacMap HM.! compound
    k | produced >= needed = 1
      | otherwise = (needed + produced - 1) `div` produced

data IntBisector =
  IntBisector
  { xm1 :: Int
  , ym1 :: Int
  , xm2 :: Int
  , ym2 :: Int
  }

data BisectionCase = AllLower | AllGreater | Surrounded deriving Show

maxFuelProduced :: ReactionMap -> Int -> Int
maxFuelProduced reactions amountOfOre = recurseBisect (IntBisector x0 (f x0) (2*x0) (2* (f x0)))
  where
    x0         = amountOfOre `div` oreNeeded reactions (1, "FUEL")
    f i        = oreNeeded reactions (i, "FUEL")
    recurseBisect :: IntBisector -> Int
    recurseBisect IntBisector{..} =
      let
        (x, bcase) = if
          | ym1 < amountOfOre && ym2 < amountOfOre -> (2 * xm1            , AllLower  )
          | ym1 > amountOfOre && ym2 > amountOfOre -> (xm1 `div` 2        , AllGreater)
          | otherwise                              -> ((xm1 + xm2) `div` 2, Surrounded)
        y = f x
        (xm2', ym2') = case bcase of
          Surrounded -> if y < amountOfOre
                          then if ym1 > amountOfOre then (xm1, ym1) else (xm2, ym2)
                          else if ym1 < amountOfOre then (xm1, ym1) else (xm2, ym2)
          _          -> (xm1, ym1)
      in if
        | y == amountOfOre || x == (min xm1 xm2) -> x
        | otherwise -> recurseBisect IntBisector{xm1=x,ym1=y,xm2=xm2',ym2=ym2'}


part1 :: IO ()
part1 = do
  let ex1 = HM.fromList . fmap parseOneRow $ smallExample1
  putStrLn $ "1st small example: " ++ (tshow $ oreNeeded ex1 (1, "FUEL"))
  let ex2 = HM.fromList . fmap parseOneRow $ smallExample2
  putStrLn $ "2nd small example: " ++ (tshow $ oreNeeded ex2 (1, "FUEL"))
  lex1 <- loadReactions "day14/large_example_1.csv"
  putStrLn $ "1st large example: " ++ (tshow $ oreNeeded lex1 (1, "FUEL"))
  lex2 <- loadReactions "day14/large_example_2.csv"
  putStrLn $ "2nd large example: " ++ (tshow $ oreNeeded lex2 (1, "FUEL"))
  lex3 <- loadReactions "day14/large_example_3.csv"
  putStrLn $ "3rd large example: " ++ (tshow $ oreNeeded lex3 (1, "FUEL"))
  reactions <- loadReactions "day14/puzzle_input.csv"
  putStrLn $ "Puzzle solution: " ++ (tshow $ oreNeeded reactions (1, "FUEL"))



part2 :: IO ()
part2 = do
  let oneTrillion :: Int
      oneTrillion = 10^(12::Int)
  lex1 <- loadReactions "day14/large_example_1.csv"
  putStrLn $ "1st large example: " ++ (tshow $ maxFuelProduced lex1 oneTrillion)
  lex2 <- loadReactions "day14/large_example_2.csv"
  putStrLn $ "2nd large example: " ++ (tshow $ maxFuelProduced lex2 oneTrillion)
  lex3 <- loadReactions "day14/large_example_3.csv"
  putStrLn $ "3rd large example: " ++ (tshow $ maxFuelProduced lex3 oneTrillion)
  reactions <- loadReactions "day14/puzzle_input.csv"
  putStrLn $ "Puzzle solution: " ++ (tshow $ maxFuelProduced reactions oneTrillion)

loadReactions :: FilePath -> IO ReactionMap
loadReactions fp = do
  rows :: [Text] <- T.lines <$> readFile fp
  return $ HM.fromList $ parseOneRow <$> rows

parseOneRow :: Text -> (Compound, (Int, [Reactive]))
parseOneRow row = (compound, (stoichR, leftReactives))
  where
    [lhs, rhs]                 = T.splitOn "=>" row
    (stoichR, compound)        = toReactive . separate $ rhs
    leftReactives              = fmap (toReactive . separate) . T.splitOn ", " $ lhs
    separate                   = T.splitOn " " . T.strip
    toReactive [stoichL, reac] = (read stoichL, reac)
    toReactive t               = error $ "Ill formed input text: " ++ show t

smallExample1, smallExample2 :: [Text]
smallExample1 =
  [ "10 ORE => 10 A"
  , "1 ORE => 1 B"
  , "7 A, 1 B => 1 C"
  , "7 A, 1 C => 1 D"
  , "7 A, 1 D => 1 E"
  , "7 A, 1 E => 1 FUEL"
  ]
smallExample2 =
  [ "9 ORE => 2 A"
  , "8 ORE => 3 B"
  , "7 ORE => 5 C"
  , "3 A, 4 B => 1 AB"
  , "5 B, 7 C => 1 BC"
  , "4 C, 1 A => 1 CA"
  , "2 AB, 3 BC, 4 CA => 1 FUEL"
  ]
