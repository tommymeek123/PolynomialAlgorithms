-----------------------------------------------------------------------------------------
-- |
-- Authors : Tommy Meek and Frank Moore
--
-- This module contains a sparse representation of monomials. The data here is
-- stored as an IntMap of Ints where i maps to the exponent of x_i.
-----------------------------------------------------------------------------------------
module SparseMonom ( Monomial
--                  , divides
--                  , divideBy
--                  , fromList
--                  , gcdMon
--                  , lcmMon
                  , multiDegree
--                  , totalDegree
                  ) where

import GHC.TypeLits (Symbol, Nat, KnownNat)
import Data.Proxy (Proxy(..))
import Data.Reflection (reflect)
import qualified Data.Vector.Fixed as V
import qualified Data.IntMap as IMap
import qualified RingParams as RP
import PolyParsers (Readable(..), monMapFromString, monListToString)

-- Type synonym
type Mon = Monomial

-- | A commutative monomial. Exponents are stored in an IntMap.
newtype Monomial :: Nat -> RP.MonOrder -> * where
    MakeMon :: { degMap :: IMap.IntMap Int } -> Monomial n o

deriving instance V.Arity n => Eq (Mon n o)

instance V.Arity n => Show (Mon n o) where
    show = monListToString . multiDegree

-- | A list of the exponents of the variables in a monomial
multiDegree :: V.Arity n => Mon n o -> [Int]
multiDegree = IMap.elems . degMap
