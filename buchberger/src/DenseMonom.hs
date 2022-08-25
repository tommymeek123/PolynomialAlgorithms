{-# LANGUAGE RankNTypes #-}

module DenseMonom (Mon(..)
                 , MonOrder(..)
                 , totalMonDeg
                 , format
                 , fromString
                 , monMult) where

import Data.List
import Data.List.Split
import Data.Char.SScript
import Data.Char
import Debug.Trace

data MonOrder = Lex | Grlex | Grevlex deriving (Eq, Show)

data Mon = Mon { degList :: [Int]
               , monOrder :: MonOrder
               } deriving (Eq)

instance Ord Mon where
    compare a b = case monOrder a of Lex -> lexOrder a b
                                     Grlex -> grlexOrder a b
                                     Grevlex -> grevlexOrder a b

instance Show Mon where
    show = format

lexOrder :: Mon -> Mon -> Ordering
lexOrder a b = compare (degList a) (degList b)

grlexOrder :: Mon -> Mon -> Ordering
grlexOrder a b = 
    let aVb = compare (totalMonDeg a) (totalMonDeg b)
    in  if aVb == EQ
        then lexOrder a b
        else aVb

grevlexOrder :: Mon -> Mon -> Ordering
grevlexOrder a b = 
    let aVb = compare (totalMonDeg a) (totalMonDeg b)
        degDiff = zipWith (-) (degList a) (degList b)
    in  if aVb == EQ
        then (compare 0) . headOrZero . dropWhile (==0) . reverse $ degDiff
        else aVb

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

totalMonDeg :: Mon -> Int
totalMonDeg = sum . degList

format :: Mon -> String
format m = formatSS . concat . snd $ mapAccumL f 1 (degList m)
    where f n x | x == 0 = (n+1, "")
                | x == 1 = (n+1, "x_" ++ show n)
                | otherwise = (n+1, "x_" ++ show n ++ "^" ++ show x)

polyListFromString :: Int -> String -> [Int]
polyListFromString n = (rpad n) . (foldl f []) . sort . (splitOn "x_") . (filter $ not . isSpace)
    where f acc s   | s == "" = acc
                    | length acc + 1 < (digitToInt . head) s = f (acc ++ [0]) s
                    | '^' `notElem` s = acc ++ [1]
                    | otherwise = acc ++ [(digitToInt . last) s]

rpad :: Int -> [Int] -> [Int]
rpad m xs = take m $ xs ++ repeat 0

fromString :: String -> Int -> MonOrder -> Mon
fromString s n o = Mon (polyListFromString n s) o

monMult :: Mon -> Mon -> Mon
monMult x y = Mon (mapComp (zipWith (+)) degList x y) (monOrder x)
--monMult x y = Mon (zipWith (+) (degList x) (degList y)) (monOrder x)

mapComp :: (k -> k -> b) -> (a -> k) -> a -> a -> b
mapComp g h x y = g (h x) (h y)

