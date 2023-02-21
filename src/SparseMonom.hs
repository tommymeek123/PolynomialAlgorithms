-----------------------------------------------------------------------------------------
-- |
-- Authors : Tommy Meek and Frank Moore
--
-- This module contains a sparse representation of monomials. The data here is
-- stored as an IntMap of Ints where i maps to the exponent of x_i.
-----------------------------------------------------------------------------------------
module SparseMonom ( Monomial
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
import Data.Vector.Fixed (Arity)
import qualified Data.IntMap as IMap
import qualified RingParams as RP
import PolyParsers (Readable(..), monMapFromString, monListToString)
import Debug.Trace (trace, traceShow)

-- Type synonym
type Mon = Monomial

-- | A commutative monomial. Exponents are stored in an IntMap.
newtype Monomial :: Nat -> RP.MonOrder -> * where
    MakeMon :: { degMap :: IMap.IntMap Int } -> Monomial n o deriving Eq

makeMon :: IMap.IntMap Int -> Mon n o
makeMon = MakeMon . IMap.filterWithKey (\k _ -> k /= 0)

lexCompare :: Mon n o -> Mon n o -> Ordering
lexCompare a b = compare (degList a) (degList b)
    where degList = IMap.assocs . degMap

revLexCompare :: Mon n o -> Mon n o -> Ordering
revLexCompare a b = compare (revList b) (revList a)
    where revList = reverse . IMap.assocs . degMap

instance Ord (Mon n RP.Lex) where
    compare = lexCompare

instance Ord (Mon n RP.GLex) where
    compare a b = let aVb = compare (totalDegree a) (totalDegree b)
                  in  if aVb == EQ
                      then lexCompare a b
                      else aVb

instance Ord (Mon n RP.GRevLex) where
    compare a b = let aVb = compare (totalDegree a) (totalDegree b)
                  in  if aVb == EQ
                      then revLexCompare a b
                      else aVb

instance Show (Mon n o) where
    show = monListToString . multiDegree

instance Semigroup (Mon n o) where
    a <> b = MakeMon $ IMap.unionWith (+) (degMap a) (degMap b)

instance Monoid (Mon n o) where
    mempty = MakeMon $ IMap.empty

instance Arity n => Readable (Mon n o) where
    fromString = MakeMon . monMapFromString nn
        where nn = (fromInteger . reflect) (Proxy :: Proxy n)

-- | Determines if the first argument divides the second argument
divides :: Mon n o -> Mon n o -> Bool
a `divides` b = all (`varDivides` (degList b)) (degList a)
    where degList = IMap.assocs . degMap
          varDivides (n1,e1) ((n2,e2):vars) = if n1 > n2
                                              then varDivides (n1,e1) vars
                                              else n1 == n2 && e1 <= e2

-- | Given monomials b and a, returns a monomial d such that b = ad
divideBy :: Mon n o -> Mon n o -> Maybe (Mon n o)
b `divideBy` a = if a `divides` b then Just (makeMon diff) else Nothing
    where diff = IMap.unionWith (-) (degMap b) (degMap a)

-- | The GCD of two monomials
gcdMon :: Mon n o -> Mon n o -> Mon n o
gcdMon a b = MakeMon $ IMap.intersectionWith (min) (degMap a) (degMap b)

-- | The LCM of two monomials
lcmMon :: Mon n o -> Mon n o -> Mon n o
lcmMon a b = MakeMon $ IMap.unionWith (max) (degMap a) (degMap b)

-- | Creates a monomial from a list of exponents.
fromList :: [Int] -> Mon n o
fromList = makeMon . IMap.fromList . zip [1..]

-- | A list of the exponents of the variables in a monomial
multiDegree :: Mon n o -> [Int]
multiDegree = IMap.foldlWithKey f [] . degMap
--    where f lst k exp = if length (trace ("k="++show k ++ "lst=" ++ show lst) lst) == k-1 then exp:lst else 0:lst
    where f lst k exp = if length lst == k-1 then lst ++ [exp] else lst ++ [0]

-- | The sum of the exponents of the variables in a monomial
totalDegree :: Mon n o -> Int
totalDegree = sum . IMap.elems . degMap
