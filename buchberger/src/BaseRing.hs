{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module BaseRing ( Field(..)
                , fromString
                ) where

import Data.Char (digitToInt)
import Data.Ratio ((%), numerator, denominator)
--import Control.DeepSeq (NFData, rnf)
import qualified RingParams as RP

data Field = Q Rational | Fp deriving (Eq,Ord,Num,Fractional)

instance Show Field where
    show (Q r) = if d == 1
                 then show n
                 else show n ++ "/" ++ show d
                 where n = numerator r
                       d = denominator r
    show (Fp) = show RP.Fp

--instance NFData Field where
--    rnf (Q a) = rnf a

fromString :: RP.RingParams -> String -> Field
fromString r = case RP.field r of RP.Q  -> ratFromString
                                  RP.Fp -> fpFromString

ratFromString :: String -> Field
ratFromString [] = Q $ 1 % 1
ratFromString xs = if '/' `elem` xs
                   then Q $ n % d
                   else Q $ read xs % 1
                   where n = read $ takeWhile (/= '/') xs
                         d = read . tail $ dropWhile (/= '/') xs

fpFromString :: String -> Field
fpFromString = \_ -> Fp
