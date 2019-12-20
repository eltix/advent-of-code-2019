{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Preprocessing
  ( readProgram
  ) where

import BasicPrelude
import qualified Data.Text as T

readProgram :: FilePath -> IO [Int]
readProgram fp = fmap read . T.splitOn "," . head . T.lines <$> readFile fp
