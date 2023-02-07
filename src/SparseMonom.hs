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
                  , totalDegree
                  ) where

import GHC.TypeLits (Symbol, Nat, KnownNat)
import Data.Proxy (Proxy(..))
import Data.Reflection (reflect)
import Data.Vector.Fixed (Arity)
import qualified Data.IntMap as IMap
import qualified RingParams as RP
import PolyParsers (Readable(..), monMapFromString, monListToString)

-- Type synonym
type Mon = Monomial

-- | A commutative monomial. Exponents are stored in an IntMap.
newtype Monomial :: Nat -> RP.MonOrder -> * where
    MakeMon :: { degMap :: IMap.IntMap Int } -> Monomial n o

deriving instance Arity n => Eq (Mon n o)

lexCompare :: Mon n o -> Mon n o -> Ordering
lexCompare a b = compare (assocList a) (assocList b)
    where assocList = IMap.assocs . degMap

revLexCompare :: Mon n o -> Mon n o -> Ordering
revLexCompare a b = compare (revList b) (revList a)
    where revList = reverse . IMap.assocs . degMap

instance Arity n => Ord (Mon n RP.Lex) where
    compare = lexCompare

instance Arity n => Ord (Mon n RP.GLex) where
    compare a b = let aVb = compare (totalDegree a) (totalDegree b)
                  in  if aVb == EQ
                      then lexCompare a b
                      else aVb

instance Arity n => Ord (Mon n RP.GRevLex) where
    compare a b = let aVb = compare (totalDegree a) (totalDegree b)
                  in  if aVb == EQ
                      then revLexCompare a b
                      else aVb

instance Arity n => Show (Mon n o) where
    show = monListToString . multiDegree

instance Arity n => Semigroup (Mon n o) where
    a <> b = MakeMon $ IMap.unionWith (+) (degMap a) (degMap b)

instance Arity n => Monoid (Mon n o) where
    mempty = MakeMon $ IMap.empty

instance Arity n => Readable (Mon n o) where
    fromString = MakeMon . monMapFromString nn
        where nn = (fromInteger . reflect) (Proxy :: Proxy n)

-- | Creates a monomial from a list of exponents.
fromList :: Arity n => [Int] -> Mon n o
fromList = MakeMon . IMap.fromList . filter (\(a,_) -> a /= 0) . zip [1..]

-- | A list of the exponents of the variables in a monomial
multiDegree :: Arity n => Mon n o -> [Int]
multiDegree = IMap.foldrWithKey f [] . degMap
    where f k exp lst = if length lst == k+1 then exp:lst else 0:lst

-- | The sum of the exponents of the variables in a monomial
totalDegree :: Arity n => Mon n o -> Int
totalDegree = sum . IMap.elems . degMap
