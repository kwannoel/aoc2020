#!/usr/bin/env runghc

{-

---------- PROBLEM

Given a list of numbers: find 2 numbers that add up to 2020, return their product

---------- SOLUTION

Hash numbers, perform lookup & shortcircuit after finding the first match

-}

import           Control.Arrow ((>>>))
import           Data.Either   (fromRight)
import           Data.Set      (empty, insert, member)
import           Text.Read     (readEither)

main :: IO ()
main = interact
     $ words
   >>> traverse readEither
   >>> fromRight (error "Input has non-integers")
   >>> foldr f (Left empty)
   >>> fromRight (error "No matches")
   >>> show
   where
       f i (Left s) | (2020 - i) `member` s = Right $ i * (2020 - i)
                    | otherwise = Left $ i `insert` s
       f _ v = v
