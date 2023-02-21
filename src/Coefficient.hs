{-# LANGUAGE StrictData #-}
-----------------------------------------------------------------------------------------
-- |
-- Authors : Tommy Meek and Frank Moore
--
-- This is a module for coefficients in a polynomial ring. The types here
-- should be rings at least. Most will likely be fields.
-----------------------------------------------------------------------------------------
module Coefficient ( Coefficient ) where

import Data.Ratio ((%), numerator, denominator)
--import Control.DeepSeq (NFData, rnf)
import qualified RingParams as RP
import PolyParsers (Readable(..), ratFromString, ratToString)

-- Type synonyms
type Q = Coefficient RP.Q
type Fp = Coefficient RP.Fp
--type Fq = Coefficient RP.Fq

-- | A coefficient in a polynomial ring
data Coefficient :: RP.Ring -> * where
    Q :: Rational -> Coefficient r
    Fp :: Coefficient r
--    Fq :: Integer -> Coefficient r
    deriving Eq

instance Show Q where
    show (Q r) = ratToString r

--instance NFData Field where
--    rnf (Q a) = rnf a

instance Num Q where
    (Q r) + (Q s) = Q (r + s)
    (Q r) - (Q s) = Q (r - s)
    (Q r) * (Q s) = Q (r * s)
    abs (Q r)     = Q (abs r)
    signum (Q r)  = Q (signum r)
    fromInteger n = Q $ n % 1

instance Fractional Q where
    recip (Q r)  = Q (denominator r % numerator r)
    fromRational = Q

instance Readable Q where
    fromString = Q . ratFromString

instance Show Fp where
    show _ = show RP.Fp

instance Num Fp where
    a + b         = Fp
    a - b         = Fp
    a * b         = Fp
    abs a         = Fp
    signum a      = Fp
    fromInteger n = Fp

instance Fractional Fp where
    recip a        = Fp
    fromRational a = Fp

instance Readable Fp where
    fromString s = Fp

--deriving instance Show Fq

--instance Num Fq where
--    (Fq r) + (Fq s) = Fq ((r + s) `mod` 2)
--    (Fq r) - (Fq s) = Fq ((r - s) `mod` 2)
--    (Fq r) * (Fq s) = Fq ((r * s) `mod` 2)
--    abs (Fq r)     = Fq (abs r)
--    signum (Fq r)  = Fq (signum r)
--    fromInteger n = Fq $ n `mod` 2

--instance Fractional Fq where
--    recip 1        = Fq 1
--    fromRational a = fromInteger (numerator a) * recip (fromInteger (denominator a))

--instance Readable Fq where
--    fromString = Fq . read
