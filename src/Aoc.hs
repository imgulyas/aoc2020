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
    day4,
  )
where

import Data.Text (split)
import Relude.Extra.Map
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

parametricCT :: Int -> Int -> [[Char]] -> Int
parametricCT right down = go
  where
    go [] = 0
    go (level : rest) = isHeadTree + go (drop right <$> drop (down -1) rest)
      where
        isHeadTree :: Int
        isHeadTree = bool 0 1 $ level !!? 0 == Just '#'

day3 :: [Text] -> IO ()
day3 input = do
  let slideMap = cycle . toString <$> input
  let solution1 = parametricCT 3 1 slideMap
  let task2params = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  let solution2 = product $ (\(r, d) -> parametricCT r d slideMap) <$> task2params
  print solution1
  print solution2

joinBlocks :: [Text] -> [Text]
joinBlocks [] = []
joinBlocks [""] = []
joinBlocks [ne] = [ne]
joinBlocks ("" : rest) = joinBlocks rest
joinBlocks (ne : "" : rest) = ne : joinBlocks rest
joinBlocks (ne : ne2 : rest) = joinBlocks $ (ne <> " " <> ne2) : rest

parseBlocks :: [Text] -> [Map Text Text]
parseBlocks blocks =
  let tuplify :: [Text] -> Maybe (Text, Text)
      tuplify [t1, t2] = Just (t1, t2)
      tuplify _ = Nothing

      blockToPairs :: Text -> [Maybe (Text, Text)]
      blockToPairs block = tuplify . take 2 . split (== ':') <$> words block

      mapFromPairs :: [Maybe (Text, Text)] -> Map Text Text
      mapFromPairs maybes = fromList . catMaybes $maybes
   in mapFromPairs . blockToPairs <$> blocks

passportFields :: [Text]
passportFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

validDocument :: Map Text Text -> Bool
validDocument doc = and $ (\k -> member k doc) <$> take 7 passportFields

day4 :: [Text] -> IO ()
day4 input = do
  let docs = parseBlocks . joinBlocks $ input
  let solution1 = length . filter validDocument $ docs
  print solution1
