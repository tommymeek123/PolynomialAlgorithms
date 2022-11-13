module PolyParsers ( monListFromString ) where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Char (digitToInt, isSpace)

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

rpad :: Int -> [Int] -> [Int]
rpad m xs = take m $ xs ++ repeat 0
