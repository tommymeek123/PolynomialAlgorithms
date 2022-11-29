module PolyParsers ( Readable(..)
                   , monListFromString
                   , polyTupleListFromString
                   , ratFromString) where

import Data.List (intercalate, sort)
import Data.List.Split (splitOn)
import Data.Char (digitToInt, isSpace)
import Data.Tuple (swap)
import Data.Ratio ((%))

-- Class for parsing from a String. Ideally, this should be replaced by Parsec.
class  Readable a  where
    {-# MINIMAL fromString #-}
    fromString :: String -> a

ratFromString :: String -> Rational
ratFromString [] = 1 % 1
ratFromString xs = if '/' `elem` xs
                   then n % d
                   else read xs % 1
                   where n = read $ takeWhile (/= '/') xs
                         d = read . tail $ dropWhile (/= '/') xs

monListFromString :: Int -> String -> [Int]
monListFromString n = rpad n
                    . foldl f []
                    . sort
                    . splitOn "x_"
                    . filter (not . isSpace)
    where f acc s | s == "" = acc
                  | length acc + 1 < (read . head . splitOn "^") s = f (acc ++ [0]) s
                  | '^' `notElem` s = acc ++ [1]
                  | otherwise = acc ++ [(read . last . splitOn "^") s]

polyTupleListFromString :: String -> [(String, String)]
polyTupleListFromString = map (swap . break (=='x'))
                        . splitOn "+" -- Make a list of terms
                        . intercalate "+-"
                        . filter (not . null)
                        . splitOn "-" -- This handles subtraction
                        . filter (not . isSpace)

rpad :: Int -> [Int] -> [Int]
rpad m xs = take m $ xs ++ repeat 0
