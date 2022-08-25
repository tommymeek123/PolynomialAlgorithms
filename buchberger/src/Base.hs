{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Base (Q(..)) where

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

numeratorQ (Q x) = Data.Ratio.numerator x
denominatorQ (Q x) = Data.Ratio.denominator x
