{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils
  ( readProgram
  , loadIntList
  , stringToIntList
  , chunksOf
  ) where

import BasicPrelude
import qualified Prelude
import qualified Data.Text as T

readProgram :: FilePath -> IO [Int]
readProgram fp = fmap read . T.splitOn "," . head . T.lines <$> readFile fp

loadIntList :: FilePath -> IO [Int]
loadIntList fp = do
  s :: String <- init . T.unpack <$> readFile fp
  return $ stringToIntList s

stringToIntList :: String -> [Int]
stringToIntList s = Prelude.read <$> [[c] | c <- s]

-- | Can't believe this isn't in the standard library :(
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = (take n l) : (chunksOf n (drop n l))
