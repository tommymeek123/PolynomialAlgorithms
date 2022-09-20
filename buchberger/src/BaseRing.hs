{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module BaseRing ( Field(..)
                , fromString
                ) where

import Data.Char (digitToInt)
import Data.Ratio
import Control.DeepSeq
import Text.Read
import qualified RingParams

data Field = Q Rational | Fp deriving (Eq,Ord,Num,Fractional)

instance Show Field where
    show (Q r) = if d == 1
                 then show n
                 else show n ++ "/" ++ show d
                 where n = numerator r
                       d = denominator r
--    show (Fp n p) = show (m `mod` p)

--instance Read Q where
--    readPrec = ratFromString

--instance NFData Q where
--    rnf (Q a) = rnf a

fromString :: RingParams.RingParams -> String -> Field
fromString r = case RingParams.field r of RingParams.Q  -> ratFromString
                                          RingParams.Fp -> fpFromString

ratFromString :: String -> Field
ratFromString [] = Q $ 1 % 1
ratFromString xs = if '/' `elem` xs
                then Q $ n % d
                else Q $ read xs % 1
                where n = read $ takeWhile (/= '/') xs
                      d = read . tail $ dropWhile (/= '/') xs

fpFromString :: String -> Field
fpFromString = \_ -> Fp
