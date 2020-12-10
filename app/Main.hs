module Main (main) where

import Aoc (day1)
import System.Environment

getInputList :: IO [Int]
getInputList = do
  args <- getArgs
  let maybeInputPath = head <$> nonEmpty args
  let path = case maybeInputPath of
        Just p -> p
        Nothing -> error "No input file"
  content <- readFileText path
  let maybeInts = readMaybe . toString <$> lines content :: [Maybe Int]
  pure $ catMaybes maybeInts

solveDay1 :: IO ()
solveDay1 = getInputList >>= day1

main :: IO ()
main = solveDay1
