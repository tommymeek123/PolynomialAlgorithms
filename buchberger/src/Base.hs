{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Base (Q(..)
            , ratFromString) where

import Data.Char (digitToInt)
import Data.Ratio
import Control.DeepSeq

-- |Q is just the rationals, but with a better show function than in Prelude.
newtype Q = Q Rational deriving (Eq,Ord,Num,Fractional)

instance Show Q where
    show (Q x) | b == 1    = show a
               | otherwise = show a ++ "/" ++ show b
               where a = numerator x
                     b = denominator x

instance NFData Q where
    rnf (Q a) = rnf a

--fromString :: String -> Q
--fromString [] = Q $ 1 % 1
--fromString (x:'/':y:[]) = Q $ (toInteger . digitToInt) x % (toInteger . digitToInt) y
--fromString (x:[]) = Q $ (toInteger . digitToInt) x % 1

ratFromString :: String -> Q
ratFromString [] = Q $ 1 % 1
ratFromString xs = if '/' `elem` xs
                then Q $ n % d
                else Q $ read xs % 1
                where n = read $ takeWhile (/= '/') xs
                      d = read . tail $ dropWhile (/= '/') xs

numeratorQ (Q x) = Data.Ratio.numerator x
denominatorQ (Q x) = Data.Ratio.denominator x
