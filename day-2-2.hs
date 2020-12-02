#!/usr/bin/env runghc

{-

---------- PROBLEM

Check if the string contains the correct number of letters using the constraint

---------- SOLUTION

-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable (foldl')
import           Control.Arrow              ((>>>))
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Either                (fromRight)
import Control.Monad (void)
import qualified Text.Parsec                as P

type Input = [InputLine]
data InputLine = InputLine
    { pos1 :: Int
    , pos2 :: Int
    , letter :: Char
    , password :: String
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
        f count InputLine{..} | atPos1 `xor` atPos2 = (1 + count)
                              | otherwise = count
            where
                atPos1 = (password !! (pos1 - 1)) == letter
                atPos2 = (password !! (pos2 - 1)) == letter
                xor = (/=)

-- | Input: <pos1>-<pos2> <letter>: <string-of-letters>
parse :: BS.ByteString -> Input
parse = fromRight (error "Malformed file") . P.runParser inputP () ""
    where
        inputP = P.sepEndBy lineP P.space

        lineP = do
            pos1 <- intP
            dashP
            pos2 <- intP
            P.spaces
            letter <- P.letter
            colonP
            P.spaces
            password <- passwordP
            return $ InputLine {..}

        passwordP = P.many1 P.letter
        intP = read <$> P.many1 P.digit
        dashP = void $ P.char '-'
        colonP = void $ P.char ':'
