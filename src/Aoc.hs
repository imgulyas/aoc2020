-- |
-- Copyright: (c) 2020 Imre Gulyas
-- SPDX-License-Identifier: MIT
-- Maintainer: Imre Gulyas <imgulyas@gmail.com>
--
-- See README for more info
module Aoc
  ( day1,
    day2,
  )
where

import Text.Parsec

day1 :: [Int] -> IO ()
day1 input =
  let solutions1 = [x * y | x <- input, y <- input, x + y == 2020]
      solutions2 = [x * y * z | x <- input, y <- input, z <- input, x + y + z == 2020]
   in do
        putStrLn "Solution 1"
        print $ head <$> nonEmpty solutions1
        putStrLn "Solution 2"
        print $ head <$> nonEmpty solutions2

data PasswordEntry = PasswordEntry Int Int Char String
  deriving (Show)

passwordEntryParser :: ParsecT Text u Identity PasswordEntry
passwordEntryParser = do
  minCount <- many1 digit
  char '-'
  maxCount <- many1 digit
  space
  checkedChar <- anyChar
  char ':'
  space
  password <- many1 anyChar
  let (minInt, maxInt) = asInt minCount maxCount
  pure $ PasswordEntry minInt maxInt checkedChar password
  where
    helper min max = do
      a <- readMaybe min :: Maybe Int
      b <- readMaybe max :: Maybe Int
      Just (a, b)

    asInt min max =
      case helper min max of
        Just p -> p
        Nothing -> error "impossible"

checkEntry :: PasswordEntry -> Bool
checkEntry (PasswordEntry lower upper c password) =
  let count :: Int
      count = length $ filter (== c) password
   in lower <= count && count <= upper

day2 :: [Text] -> IO ()
day2 input = do
  let parseEntry = parse passwordEntryParser ""
  let parsedEntries = sequence $ parseEntry <$> input
  case parsedEntries of
    Right entries -> print . length . filter (== True) $ checkEntry <$> entries
    Left e -> error $ show e
