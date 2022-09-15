{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BaseRing (fromString) where

import Data.Char (digitToInt)
import Data.Ratio
import Control.DeepSeq
import Text.Read

instance Show Field where
    show (Q r) = if d == 1
                 then show n
                 else show n ++ "/" ++ show d
                 where n = numerator r
                       d = denominator r
    show (Fp n p) = show (m `mod` p)

--instance Read Q where
--    readPrec = ratFromString

instance NFData Q where
    rnf (Q a) = rnf a

reduceMod :: Int -> Int -> Int
n `reduceMod` p = n `mod` p

ratFromString :: String -> Q
ratFromString [] = Q $ 1 % 1
ratFromString xs = if '/' `elem` xs
                then Q $ n % d
                else Q $ read xs % 1
                where n = read $ takeWhile (/= '/') xs
                      d = read . tail $ dropWhile (/= '/') xs
