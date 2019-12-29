{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( part1
  , part2
  ) where

import BasicPrelude hiding (Vector)

import Control.Lens
import Data.Semigroup

newtype Vector = Vector {getVector :: (Int, Int, Int)} deriving Eq

instance Show Vector where
  show (Vector (x, y, z)) = "<x=" ++ show x ++ ", y="++ show y ++ ", z="++ show z ++ " >"

instance Num Vector where
  Vector (xa, ya, za) + Vector (xb, yb, zb) = Vector (xa+xb, ya+yb, za+zb)
  Vector (xa, ya, za) - Vector (xb, yb, zb) = Vector (xa-xb, ya-yb, za-zb)
  (*) = undefined
  fromInteger = undefined
  abs = undefined
  signum (Vector (x, y, z)) = Vector (signum x, signum y, signum z)

instance Semigroup Vector where
  (<>) = (+)

instance Monoid Vector where mempty = Vector (0, 0, 0)

type System = [Moon]

data Moon =
  Moon
  { _mid      :: Int
  , _position :: Vector
  , _velocity :: Vector
  } deriving Eq

instance Show Moon where
  show (Moon _ p v) = "pos=" ++ show p ++ ", vel=" ++ show v

makeLenses ''Moon

newMoon :: Int -> Int -> Int -> Int -> Moon
newMoon i x y z = Moon i (Vector (x, y, z)) (Vector (0, 0, 0))

(@->) :: Moon -> Moon -> Vector
ma @-> mb = signum $ ma ^. position - mb ^. position

(@/=) :: Moon -> Moon -> Bool
ma @/= mb = ma ^. mid /= mb ^. mid

oneIteration :: System -> System
oneIteration = updatePosition . updateVelocity

updateVelocity :: System -> System
updateVelocity system = updateOneMoon <$> system
  where
    updateOneMoon moonA = moonA & velocity %~
      ((+) $ concat [moonB @-> moonA | moonB <- system, moonA @/= moonB])

updatePosition :: System -> System
updatePosition = fmap updateOneMoon
  where
    updateOneMoon moon = moon & position %~ ((+) $ moon ^. velocity)

kineticEnergy :: Moon -> Int
kineticEnergy = (\(x, y, z) -> abs x + abs y + abs z) . getVector . view velocity

potentialEnergy :: Moon -> Int
potentialEnergy = (\(x, y, z) -> abs x + abs y + abs z) . getVector . view position

systemEnergy :: System -> Int
systemEnergy = sum . fmap (\moon -> kineticEnergy moon * potentialEnergy moon)

findRepeatedStateOnAxis :: System -> Int -> (Int, System) -> (Int, System)
findRepeatedStateOnAxis system0 axis (i, system)
  | equalOnAxis axis (oneIteration system) (system0) = (i+1, system')
  | otherwise                      = findRepeatedStateOnAxis system0 axis (i+1, system')
  where system' = oneIteration system

equalOnAxis :: Int -> System -> System -> Bool
equalOnAxis axis system0 = all (equalMoon axis) . zip system0
  where
    equalMoon 0 (Moon _ (Vector (xa,_,_)) (Vector (ua,_,_)), Moon _ (Vector (xb,_,_)) (Vector (ub,_,_))) = xa == xb && ua == ub
    equalMoon 1 (Moon _ (Vector (_,ya,_)) (Vector (_,va,_)), Moon _ (Vector (_,yb,_)) (Vector (_,vb,_))) = ya == yb && va == vb
    equalMoon 2 (Moon _ (Vector (_,_,za)) (Vector (_,_,wa)), Moon _ (Vector (_,_,zb)) (Vector (_,_,wb))) = za == zb && wa == wb
    equalMoon i _ = error $ "Axis beyond range: " ++ show i

part1 :: IO ()
part1 = do
  let
    system0       = puzzle_input
    numIterations = 1000
    forLoop     n = foldr (.) id (replicate n oneIteration)
    system'       = forLoop numIterations system0
  putStrLn "Example after 100 iterations:"
  mapM_ (putStrLn . tshow) $ forLoop 100 example
  putStrLn $ "Puzzle solution: total energy after 1000 iterations = " ++ (tshow . systemEnergy $ system')

part2 :: IO ()
part2 = do
  let
    system0 = puzzle_input
    xperiod = fst $ findRepeatedStateOnAxis system0 0 (0, system0)
    yperiod = fst $ findRepeatedStateOnAxis system0 1 (0, system0)
    zperiod = fst $ findRepeatedStateOnAxis system0 2 (0, system0)
    period  = lcm zperiod $ lcm xperiod yperiod
  putStrLn $ "Lowest common multiplier of all axes periods = " ++ tshow period


example, puzzle_input :: System
example = [m1, m2, m3, m4]
  where
    m1 = newMoon 0 (-1) 0 2
    m2 = newMoon 1 2 (-10) (-7)
    m3 = newMoon 2 4 (-8) 8
    m4 = newMoon 3 3 5 (-1)
puzzle_input =
  [ newMoon 0 17 (-7) (-11)
  , newMoon 1 1 4 (-1)
  , newMoon 2 6 (-2) (-6)
  , newMoon 3 19 11 9
  ]
