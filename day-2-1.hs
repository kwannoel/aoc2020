#!/usr/bin/env runghc

{-

---------- PROBLEM

Check if the string contains the correct number of letters using the constraint

---------- SOLUTION

Brute-force

-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable (foldl')
import           Control.Arrow              ((>>>))
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Either                (fromRight)
import Control.Monad (void)
import qualified Text.Parsec                as P

type Input = [InputLine]
data InputLine = InputLine
    { minOccurs :: Int
    , maxOccurs :: Int
    , letter :: Char
    , password :: ByteString
    } deriving Show

type Solution = Int -- ^ Number of ok passwords

main :: IO ()
main = BS.interact
     $ parse
   >>> solve
   >>> BS.pack . show

solve :: Input -> Solution
solve = foldl' f 0
    where
        f count InputLine{..} | occurrences >= minOccurs && occurrences <= maxOccurs = (1 + count)
                              | otherwise = count
            where
                occurrences = fromIntegral $ BS.count letter password

-- | Input: <minOccurs>-<maxOccurs> <letter>: <string-of-letters>
parse :: BS.ByteString -> Input
parse = fromRight (error "Malformed file") . P.runParser inputP () ""
    where
        inputP = P.sepEndBy lineP P.space

        lineP = do
            minOccurs <- intP
            dashP
            maxOccurs <- intP
            P.spaces
            letter <- P.letter
            colonP
            P.spaces
            password <- passwordP
            return $ InputLine {..}

        passwordP = BS.pack <$> P.many1 P.letter
        intP = read <$> P.many1 P.digit
        dashP = void $ P.char '-'
        colonP = void $ P.char ':'
