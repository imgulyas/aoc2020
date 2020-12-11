module Main (main) where

import Aoc
import System.Environment

getInputList :: IO [Text]
getInputList = do
  args <- getArgs
  let maybeInputPath = head <$> nonEmpty args
  let path = case maybeInputPath of
        Just p -> p
        Nothing -> error "No input file"
  lines <$> readFileText path

getNumberList :: IO [Int]
getNumberList = do
  input <- getInputList
  let maybeInts = readMaybe . toString <$> input :: [Maybe Int]
  pure $ catMaybes maybeInts

solveDay1 :: IO ()
solveDay1 = getNumberList >>= day1

solveDay2 :: IO ()
solveDay2 = getInputList >>= day2

solveDay3 :: IO ()
solveDay3 = getInputList >>= day3

main :: IO ()
main = solveDay3
