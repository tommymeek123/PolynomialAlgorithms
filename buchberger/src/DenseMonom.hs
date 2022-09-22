{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module DenseMonom ( Mon(..)
                  , fromString
                  , mult
                  , totalDeg
                  ) where

import Data.List (mapAccumL, sort)
import Data.List.Split (splitOn)
import Data.Char (digitToInt, isSpace)
--import Data.Text (split)
import qualified RingParams as RP

data Mon = Mon { order :: RP.MonOrder
               , degList :: [Int]
               } deriving (Eq)

instance Ord Mon where
    compare a b = case order a of RP.Lex     -> lexOrder a b
                                  RP.Grlex   -> grlexOrder a b
                                  RP.Grevlex -> grevlexOrder a b

lexOrder :: Mon -> Mon -> Ordering
lexOrder = mapComp compare degList

grlexOrder :: Mon -> Mon -> Ordering
grlexOrder a b =
    let aVb = compare (totalDeg a) (totalDeg b)
    in  if aVb == EQ
        then lexOrder a b
        else aVb

grevlexOrder :: Mon -> Mon -> Ordering
grevlexOrder a b =
    let aVb = compare (totalDeg a) (totalDeg b)
        degDiff = zipWith (-) (degList a) (degList b)
    in  if aVb == EQ
        then compare 0
           . headOrZero
           . dropWhile (==0)
           . reverse $ degDiff
        else aVb

instance Show Mon where
    show m = concat . snd $ mapAccumL f 1 (degList m)
        where f n x | x == 0 = (n+1, "")
                    | x == 1 = (n+1, "x_" ++ show n)
                    | otherwise = (n+1, "x_" ++ show n ++ "^" ++ show x)

fromString :: RP.RingParams -> String -> Mon
fromString r s = Mon o $ listFromString n s
    where o = RP.order r
          n = RP.numVars r

mult :: Mon -> Mon -> Mon
mult x y = Mon (order x) (zipWith (+) (degList x) (degList y))

totalDeg :: Mon -> Int
totalDeg = sum . degList

headOrZero :: [Int] -> Int
headOrZero [] = 0
headOrZero xs = head xs

--listFromString :: Int -> String -> [Int]
--listFromString n = rpad n
--                 . foldl f []
--                 . sort
--                 . splitOn "x_"
--                 . filter (not . isSpace)
--    where f acc s | s == "" = acc
--                  | length acc + 1 < (digitToInt . head) s = f (acc ++ [0]) s
--                  | '^' `notElem` s = acc ++ [1]
--                  | otherwise = acc ++ [(digitToInt . last) s]

listFromString :: Int -> String -> [Int]
listFromString n = rpad n
                 . foldl f []
                 . sort
                 . splitOn "x_"
                 . filter (not . isSpace)
    where f acc s | s == "" = acc
                  | length acc + 1 < (read . head . splitOn "^") s = f (acc ++ [0]) s
                  | '^' `notElem` s = acc ++ [1]
                  | otherwise = acc ++ [(read . last . splitOn "^") s]

mapComp :: (k -> k -> b) -> (a -> k) -> a -> a -> b
mapComp g h x y = g (h x) (h y)

rpad :: Int -> [Int] -> [Int]
rpad m xs = take m $ xs ++ repeat 0

--degDiff :: Mon -> Mon -> [Int]
--degDiff a b
--    | length da == length db = zipWith (-) da db
--    | length da < length db = (zipWith (-) da db) ++ (map (*(-1)) $ drop (length da) db)
--    | length da > length db = (zipWith (-) da db) ++ (drop (length db) da)
--    where da = degList a
--          db = degList b
