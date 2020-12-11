-- |
-- Copyright: (c) 2020 Imre Gulyas
-- SPDX-License-Identifier: MIT
-- Maintainer: Imre Gulyas <imgulyas@gmail.com>
--
-- See README for more info
module Aoc
  ( day1,
    day2,
    day3,
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

checkEntry2 :: PasswordEntry -> Bool
checkEntry2 (PasswordEntry fstPos sndPos c password) =
  let xxor :: Bool -> Bool -> Bool
      xxor True False = True
      xxor False True = True
      xxor _ _ = False
   in xxor (Just c == password !!? (fstPos -1)) (Just c == password !!? (sndPos -1))

day2 :: [Text] -> IO ()
day2 input = do
  let parseEntry = parse passwordEntryParser ""
  let parsedEntries = sequence $ parseEntry <$> input
  case parsedEntries of
    Right entries -> do
      let solution1 = length . filter checkEntry $ entries
      print solution1
      let solution2 = length . filter checkEntry2 $ entries
      print solution2
    Left e -> error $ show e

countTrees :: [[Char]] -> Int
countTrees [] = 0
countTrees (level : rest) = isHeadTree + countTrees (drop 3 <$> rest)
  where
    isHeadTree :: Int
    isHeadTree = bool 0 1 $ level !!? 0 == Just '#'

day3 :: [Text] -> IO ()
day3 input = do
  let slideMap = cycle . toString <$> input
  let solution1 = countTrees slideMap
  print solution1
