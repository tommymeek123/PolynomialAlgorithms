-----------------------------------------------------------------------------------------
-- |
-- Authors : Tommy Meek and Frank Moore
--
-- This module contains a dense representation of monomials. The data here is
-- stored as a fixed vector of Ints where the Int at position i represents the
-- exponent of x_i.
-----------------------------------------------------------------------------------------
module DenseMonom ( Monomial
                  , divides
                  , divideBy
                  , fromList
                  , gcdMon
                  , lcmMon
                  , multiDegree
                  , totalDegree
                  ) where

import GHC.TypeLits (Symbol, Nat, KnownNat)
import Data.Proxy (Proxy(..))
import Data.Reflection (reflect)
import qualified Data.Vector.Fixed as V
import qualified Data.Vector.Fixed.Unboxed as UV
import qualified RingParams as RP
import PolyParsers (Readable(..), monListFromString, monListToString)

-- Type synonym
type Mon = Monomial

-- | A commutative monomial. Exponents are stored in a fixed length vector.
newtype Monomial :: Nat -> RP.MonOrder -> * where
    MakeMon :: { degVec :: UV.Vec n Int } -> Monomial n o

deriving instance V.Arity n => Eq (Mon n o)

instance V.Arity n => Ord (Mon n RP.Lex) where
    compare a b = compare (degVec a) (degVec b)

instance V.Arity n => Ord (Mon n RP.GLex) where
    compare a b = let aVb = compare (totalDegree a) (totalDegree b)
                  in  if aVb == EQ
                      then compare (degVec a) (degVec b)
                      else aVb

instance V.Arity n => Ord (Mon n RP.GRevLex) where
    compare a b = let aVb = compare (totalDegree a) (totalDegree b)
                      a' = V.reverse $ degVec a
                      b' = V.reverse $ degVec b
                  in  if aVb == EQ
                      then compare b' a'
                      else aVb

instance V.Arity n => Show (Mon n o) where
    show = monListToString . multiDegree

instance V.Arity n => Semigroup (Mon n o) where
    a <> b = MakeMon { degVec = V.zipWith (+) (degVec a) (degVec b) }

instance (KnownNat n, V.Arity n) => Monoid (Mon n o) where
    mempty = MakeMon $ V.fromList' $ take nn (repeat 0)
        where nn = (fromInteger . reflect) (Proxy :: Proxy n)

instance (KnownNat n, V.Arity n) => Readable (Mon n o) where
    fromString :: forall n o. (KnownNat n, V.Arity n) => String -> Mon n o
    fromString s = MakeMon { degVec = V.fromList' $ monListFromString nn s }
        where nn = (fromInteger . reflect) (Proxy :: Proxy n)

-- | Determines if the first argument divides the second argument
divides :: V.Arity n => Mon n o -> Mon n o -> Bool
a `divides` b = V.and $ V.zipWith (<=) (degVec a) (degVec b)

-- | Given monomials a and b, returns a monomial d such that a = bd
divideBy :: V.Arity n => Mon n o -> Mon n o -> Maybe (Mon n o)
divideBy a b = if V.any (< 0) diff then Nothing else Just (MakeMon diff)
    where diff = V.zipWith (-) (degVec a) (degVec b)

-- | Creates a monomial from a list of exponents.
fromList :: V.Arity n => [Int] -> Mon n o
fromList = MakeMon . V.fromList

-- | The GCD of two monomials
gcdMon :: V.Arity n => Mon n o -> Mon n o -> Mon n o
gcdMon a b = MakeMon $ V.zipWith (min) (degVec a) (degVec b)

-- | The LCM of two monomials
lcmMon :: V.Arity n => Mon n o -> Mon n o -> Mon n o
lcmMon a b = MakeMon $ V.zipWith (max) (degVec a) (degVec b)

-- | A list of the exponents of the variables in a monomial
multiDegree :: V.Arity n => Mon n o -> [Int]
multiDegree = V.toList . degVec

-- | The sum of the exponents of the variables in a monomial
totalDegree :: V.Arity n => Mon n o -> Int
totalDegree = V.sum . degVec
