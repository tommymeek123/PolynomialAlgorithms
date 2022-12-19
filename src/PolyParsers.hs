module PolyParsers ( Readable(..)
                   , monListFromString
                   , monListToString
                   , monMapFromString
                   , polyTupleListFromString
                   , polyListToString
                   , ratFromString
                   , ratToString) where

import Data.List (deleteBy, intercalate, mapAccumL, sort)
import Data.List.Split (splitOn)
import Data.List.Index (setAt)
import Data.Char (digitToInt, isSpace)
import Data.Tuple (swap)
import Data.Ratio ((%), numerator, denominator)
import Data.Composition ((.:))
import qualified Data.IntMap as Map

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

ratToString :: Rational -> String
ratToString r = if d == 1
                then show n
                else show n ++ "/" ++ show d
                where n = numerator r
                      d = denominator r

monListFromString :: Int -> String -> [Int]
monListFromString n = foldl insertTuple zeros . monTupleListFromString n
    where insertTuple degs (i,deg) = setAt (i-1) deg degs
          zeros = take n (repeat 0)

monMapFromString :: Int -> String -> Map.IntMap Int
monMapFromString = Map.fromList .: monTupleListFromString

monTupleListFromString :: Int -> String -> [(Int,Int)]
monTupleListFromString n = filter (\(k,v) -> k <= n)
    . map (\s -> (k s, v s))
    . filter (not . null)
    . splitOn "x_"
    . filter (not . isSpace)
    where k s = (read . takeWhile (/= '^')) s
          v s = if '^' `elem` s
                then (read . tail . dropWhile (/= '^')) s
                else 1

monListToString :: [Int] -> String
monListToString xs = concat . snd $ mapAccumL f 1 xs
    where f n x | x == 0 = (n+1, "")
                | x == 1 = (n+1, "x_" ++ show n)
                | otherwise = (n+1, "x_" ++ show n ++ "^" ++ show x)

polyTupleListFromString :: String -> [(String, String)]
polyTupleListFromString [] = []
polyTupleListFromString s = map (swap . break (=='x'))
    . splitOn "+" -- Make a list of terms
    . intercalate "+-" . filter (not . null) . splitOn "-" -- Handle subtraction
    . filter (not . isSpace) $ s

polyListToString :: [String] -> String
polyListToString [] = "0"
polyListToString f = let isMonic s = takeWhile (/= 'x') s == "1"
                         removeOnes s = if length s > 1 && isMonic s
                                        then tail s
                                        else s
    in (intercalate " - " . splitOn " + -" -- Display subtraction
      . intercalate " + " . map removeOnes) f

--rpad :: Int -> [Int] -> [Int]
--rpad m xs = take m $ xs ++ repeat 0
