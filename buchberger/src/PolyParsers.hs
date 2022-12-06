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
monListFromString n = foldl (\acc (k,v) -> setAt (k-1) v acc) (take n (repeat 0))
    . monTupleListFromString n

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

--monListFromString :: Int -> String -> [Int]
--monListFromString n = rpad n
--                    . foldl f []
--                    . sort -- Breaks here if more than 9 variables
--                    . splitOn "x_"
--                    . filter (not . isSpace)
--    where f acc s | s == "" = acc
--                  | length acc + 1 < (read . head . splitOn "^") s = f (acc ++ [0]) s
--                  | '^' `notElem` s = acc ++ [1]
--                  | otherwise = acc ++ [(read . last . splitOn "^") s]

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
polyListToString f = let removeOnes s = if length s > 1 && takeWhile (/= 'x') s == "1"
                                        then tail s
                                        else s
                   in (intercalate " - " . splitOn " + -" -- Show - instead of + -
                     . intercalate " + " . map removeOnes) f

--rpad :: Int -> [Int] -> [Int]
--rpad m xs = take m $ xs ++ repeat 0
