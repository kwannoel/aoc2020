#!/usr/bin/env runghc

{-

---------- PROBLEM

Given a list of numbers: find 2 numbers that add up to 2020, return their product

---------- SOLUTION

Hash numbers, perform lookup & shortcircuit after finding the first match

-}

import           Control.Arrow              ((>>>))
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Either                (fromRight)
import           Data.Set                   (empty, insert, member)
import qualified Text.Parsec                as P


main :: IO ()
main = BS.interact
     $ parse
   >>> solve
   >>> BS.pack . show

solve :: [Int] -> Int
solve = fromRight (error "No matches") . foldr f (Left empty)
    where
        f i (Left s) | member (2020 - i) s = Right $ i * (2020 - i)
                     | otherwise = Left $ i `insert` s
        f _ v = v

parse :: BS.ByteString -> [Int]
parse = fromRight (error "Malformed file") . P.runParser inputP () ""
    where
        inputP = P.sepEndBy int P.space
        int = read <$> P.many1 P.digit
