{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( readProgram
  , chunksOf
  ) where

import BasicPrelude
import qualified Data.Text as T

readProgram :: FilePath -> IO [Int]
readProgram fp = fmap read . T.splitOn "," . head . T.lines <$> readFile fp

-- | Can't believe this isn't in the standard library :(
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = (take n l) : (chunksOf n (drop n l))
