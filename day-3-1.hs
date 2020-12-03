#!/usr/bin/env runghc

{-

---------- PROBLEM

Given a 2d boolean array, and a path, how many True values we hit upon reaching last row.

Note the boolean array wraps around around the x-axis:

0 0 1 0 0 1 ...
0 1 0 0 1 0 ...

Our path is 3 steps right, 1 step down, starting from top-left-most position.

---------- SOLUTION



-}


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}


import           Control.Arrow              ((>>>))
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Bits (shift, (.|.), (.&.))
import Data.Foldable (foldl')

data Input = Input
    { rows :: [Row]
    , rowLength :: Int
    } deriving (Eq, Show)
type Row = Int -- ^ Use bits to store, since length < 32

type Solution = Int -- ^ Number of TRUE

main :: IO ()
main = BS.interact
     $ parse
   >>> solve
   >>> BS.pack . show

test :: IO ()
test = do
    print $ parse ".#\n.#" == Input [2,2] 2 -- [10, 10]
    print $ parse ".#.\n.##" == Input [2, 6] 3 -- [010, 110]
    print $ solve Input{ rows = [0, 1000, 1000_000, 1000_000_000], rowLength = 10}

solve :: Input -> Solution
solve Input{..} = foldl' (uncurry <$> f) 0 $ zip [pos `mod` rowLength | pos <- [3, 6 ..]] (tail rows)
    where
        f :: Int -> Int -> Row -> Int
        f acc idx row | ((1 `shift` idx) .&. row) > 0 = acc + 1
                      | otherwise = acc

-- | Input line:
-- '.' := 0
-- '#' := 1
parse :: BS.ByteString -> Input
parse bs = Input{..}
    where
        rows = fmap toRow $ rows'
        rowLength = fromIntegral $ BS.length $ head rows'
        rows' = BS.lines bs

        toRow :: BS.ByteString -> Row
        toRow = BS.foldl' accF 0

        accF :: Int -> Char -> Int
        accF acc c = (mapBit c `shift` (rowLength - 1)) .|. (acc `shift` (-1))

        mapBit '.' = 0
        mapBit '#' = 1
        mapBit _ = error "Malformed input"
