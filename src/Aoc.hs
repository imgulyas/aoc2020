-- |
-- Copyright: (c) 2020 Imre Gulyas
-- SPDX-License-Identifier: MIT
-- Maintainer: Imre Gulyas <imgulyas@gmail.com>
--
-- See README for more info
module Aoc
  ( day1,
  )
where

day1 :: [Int] -> IO ()
day1 input =
  let solutions1 = [x * y | x <- input, y <- input, x + y == 2020]
      solutions2 = [x * y * z | x <- input, y <- input, z <- input, x + y + z == 2020]
   in do
        putStrLn "Solution 1"
        print $ head <$> nonEmpty solutions1
        putStrLn "Solution 2"
        print $ head <$> nonEmpty solutions2
