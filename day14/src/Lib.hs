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

import BasicPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

type Reactive = Text
type ReactionMap = HashMap Reactive [(Reactive, Int)]

part1 :: IO ()
part1 = do
  reactions <- loadReactions "day14/large_example_1.csv"
  return undefined

part2 :: IO ()
part2 = undefined

loadReactions :: FilePath -> IO ReactionMap
loadReactions fp = do
  rows :: [Text] <- T.lines <$> readFile fp
  return $ HM.fromList $ parseOneRow <$> rows

parseOneRow :: Text -> (Reactive, [(Reactive, Int)])
parseOneRow t = undefined
