{-# LANGUAGE RankNTypes #-}

module DenseMonom ( Mon(..)
                  , MonOrder(..)
                  , totalDeg
                  , fromString
                  , format
                  , monMult
                  ) where

import Data.List
import Data.List.Split
import Data.Char.SScript (formatSS)
import Data.Char (digitToInt, isSpace)
import Debug.Trace
import RingParams

data Mon = Mon { order :: MonOrder
               , degList :: [Int]
               } deriving (Eq)

instance Ord Mon where
    compare a b = case order a of Lex     -> lexOrder a b
                                  Grlex   -> grlexOrder a b
                                  Grevlex -> grevlexOrder a b

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
        then compare 0 . headOrZero . dropWhile (==0) . reverse $ degDiff
        else aVb

instance Show Mon where
    show m = concat . snd $ mapAccumL f 1 (degList m)
        where f n x | x == 0 = (n+1, "")
                    | x == 1 = (n+1, "x_" ++ show n)
                    | otherwise = (n+1, "x_" ++ show n ++ "^" ++ show x)

format :: Mon -> String
format = formatSS . show

--degDiff :: Mon -> Mon -> [Int]
--degDiff a b
--    | length da == length db = zipWith (-) da db
--    | length da < length db = (zipWith (-) da db) ++ (map (*(-1)) $ drop (length da) db)
--    | length da > length db = (zipWith (-) da db) ++ (drop (length db) da)
--    where da = degList a
--          db = degList b

headOrZero :: [Int] -> Int
headOrZero [] = 0
headOrZero xs = head xs

totalDeg :: Mon -> Int
totalDeg = sum . degList

listFromString :: Int -> String -> [Int]
listFromString n = rpad n . foldl f [] . sort . splitOn "x_" . filter (not . isSpace)
    where f acc s | s == "" = acc
                  | length acc + 1 < (digitToInt . head) s = f (acc ++ [0]) s
                  | '^' `notElem` s = acc ++ [1]
                  | otherwise = acc ++ [(digitToInt . last) s]

rpad :: Int -> [Int] -> [Int]
rpad m xs = take m $ xs ++ repeat 0

fromString :: RingParams -> String -> Mon
fromString r s = Mon o $ listFromString n s
    where o = monOrder r
          n = numVars r

monMult :: Mon -> Mon -> Mon
monMult x y = Mon (order x) (zipWith (+) (degList x) (degList y))

mapComp :: (k -> k -> b) -> (a -> k) -> a -> a -> b
mapComp g h x y = g (h x) (h y)
