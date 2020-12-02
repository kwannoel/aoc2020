#!/usr/bin/env runghc

{-

---------- PROBLEM

Given a list of numbers: find 3 numbers that add up to 2020, return their product

-}

main :: IO ()
main = interact $ show . constraint . fmap read . lines
     where
         constraint ns = head [a*b*c | a <- ns, b <- ns, c <- ns, a + b + c == 2020]
