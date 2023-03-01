{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------------------
-- |
-- Authors : Tommy Meek and Frank Moore
--
-- A collection of functions for converting to and from strings
-----------------------------------------------------------------------------------------
module PolyParsers ( Readable(..)
                   , monListFromString
                   , monListToString
                   , monMapFromString
                   , polyTupleListFromString
                   , polyListToString
                   , ratFromString
                   , ratToString) where

import Data.List (delete, intercalate, mapAccumL, sort)
import Data.List.Split (splitOn)
import Data.List.Index (setAt)
import Data.Char (digitToInt, isSpace)
import Data.Text (pack, unpack, replace)
import Data.Tuple (swap)
import Data.Ratio ((%), numerator, denominator)
import Data.Composition ((.:))
import qualified Data.IntMap as Map

{- | A class for types that may be parsed from a string. Ideally, this should
be replaced by a parsing library like Parsec and the Read type class. -}
class  Readable a  where
    {-# MINIMAL fromString #-}
    fromString :: String -> a

-- | Convert a string to a Rational.
ratFromString :: String -> Rational
ratFromString [] = 1 % 1
ratFromString xs = if '/' `elem` xs
                   then n % d
                   else read xs % 1
                   where n = read $ takeWhile (/= '/') xs
                         d = read . tail $ dropWhile (/= '/') xs

-- | Convert a Rational to a string. Inverse of ratFromString.
ratToString :: Rational -> String
ratToString r = if d == 1
                then show n
                else show n ++ "/" ++ show d
                where n = numerator r
                      d = denominator r

{- | Convert a string representing a monomial to a list of Ints representing
the exponents. Example: "x_1^3x_3^7x_6" becomes [3,0,7,0,0,1]. -}
monListFromString :: Int -> String -> [Int]
monListFromString n = foldl insertTuple zeros . monTupleListFromString n
    where insertTuple degs (i,deg) = setAt (i-1) deg degs
          zeros = replicate n 0

{- | Convert a string representing a monomial to an IntMap from the position
of the variable to the exponent of that variable. Example: "x_1^3x_3^7x_6"
becomes the map (1->3, 3->7, 6->1). -}
monMapFromString :: Int -> String -> Map.IntMap Int
monMapFromString = Map.fromList .: monTupleListFromString

{- Convert a string representing a monomial to an association list.
Example: "x_1^3x_3^7x_6" becomes [(1,3), (3,7), (6,1)]. -}
monTupleListFromString :: Int -> String -> [(Int,Int)]
monTupleListFromString n "1" = []
monTupleListFromString n s = filter (\(k,v) -> k <= n)
    . map (\s -> (k s, v s))
    . filter (not . null)
    . splitOn "x_"
    . filter (not . isSpace) $ s
    where k s = (read . takeWhile (/= '^')) s
          v s = if '^' `elem` s
                then (read . tail . dropWhile (/= '^')) s
                else 1

-- | Convert the exponent list of a monomial to a string. Inverse of monListFromString.
monListToString :: [Int] -> String
monListToString xs = if all (==0) xs then "1" else
                     if length xs <= 3 then (xyzFormat . getString) xs else getString xs
    where getString = concat . snd . mapAccumL f 1
          f n x | x == 0 = (n+1, "")
                | x == 1 = (n+1, "x_" ++ show n)
                | otherwise = (n+1, "x_" ++ show n ++ "^" ++ show x)

xyzFormat :: String -> String
xyzFormat = unpack . replace "x_1" "x" . replace "x_2" "y" . replace "x_3" "z" . pack

{- | Convert a String representing a polynomial to an association list where
strings representing monomials are the keys and strings representing
coefficients are the values. Example: x_1^5 + 4x_1^3x_2 - 2x_1x_2^2 + 3x_2
becomes [(x_1^5,1), (x_1^3x_2,4), (x_1x_2^2,-2), (x_2,3)] -}
polyTupleListFromString :: String -> [(String, String)]
polyTupleListFromString [] = []
polyTupleListFromString s = map (\(k,v) -> if v == "-" then (k,"-1") else (k,v))
    . map (swap . break (=='x'))
    . splitOn "+" -- Make a list of terms
    . intercalate "+-" . filter (not . null) . splitOn "-" -- For subtraction
    . filter (not . isSpace) $ s

{- | Convert an association list of a polyomial to a string. Inverse of
polyTupleListFromString. -}
polyListToString :: [(String,String)] -> String
polyListToString [] = "0"
polyListToString f = let termToString ("1",c) = c
                         termToString (m,"1") = m
                         termToString (m,"-1") = '-':m
                         termToString (m,c) = c++m
    in (intercalate " - " . splitOn " + -" -- Display subtraction
      . intercalate " + " . map termToString) f

--rpad :: Int -> [Int] -> [Int]
--rpad m xs = take m $ xs ++ repeat 0
