-----------------------------------------------------------------------------------------
-- |
-- Authors : Tommy Meek and Frank Moore
--
-- This module contains a sparse representation of monomials. The data here is
-- stored as an IntMap of Ints where i maps to the exponent of x_i.
-----------------------------------------------------------------------------------------
module SparseMonom ( Monomial
                  , coprime
                  , divides
                  , divideBy
                  , fromList
                  , gcdMon
                  , lcmMon
                  , multiDegree
                  , totalDegree
                  ) where

import GHC.TypeLits (Nat)
import Data.Proxy (Proxy(..))
import Data.Reflection (reflect)
import Data.Vector.Fixed (Arity)
import qualified Data.IntMap as IMap
import qualified RingParams as RP
import PolyParsers (Readable(..), monMapFromString, monListToString)
import Debug.Trace (trace, traceShow)

-- | A commutative monomial. Exponents are stored in an IntMap.
newtype Monomial :: Nat -> RP.MonOrder -> * where
    MakeMon :: { degMap :: IMap.IntMap Int } -> Monomial n o deriving Eq

-- Type synonym
type Mon = Monomial

makeMon :: IMap.IntMap Int -> Mon n o
makeMon = MakeMon . IMap.filterWithKey (\_ exp -> exp /= 0)

lexCompare :: Mon n o -> Mon n o -> Ordering
a `lexCompare` b = (degList a) `compList` (degList b) where
    degList = IMap.assocs . degMap

revLexCompare :: forall n o. Arity n => Mon n o -> Mon n o -> Ordering
a `revLexCompare` b = (revList b) `compList` (revList a) where
    revList = map (\(x,y) -> (nn - 1 - x,y)) . reverse . IMap.assocs . degMap
    nn = (fromInteger . reflect) (Proxy :: Proxy n)

compList :: [(Int,Int)] -> [(Int,Int)] -> Ordering
compList [] [] = EQ
compList _  [] = GT
compList [] _  = LT
compList ((n1,e1):vars1) ((n2,e2):vars2)
    | n1 /= n2  = n2 `compare` n1
    | e1 /= e2  = e1 `compare` e2
    | otherwise = vars1 `compList` vars2

{--
lexCompare :: forall n o. Arity n => Mon n o -> Mon n o -> Ordering
a `lexCompare` b = compMap [1..nn] (degMap a) (degMap b)
    where nn = (fromInteger . reflect) (Proxy :: Proxy n)

revLexCompare :: forall n o. Arity n => Mon n o -> Mon n o -> Ordering
a `revLexCompare` b = compMap [nn,nn-1..1] (degMap b) (degMap a)
    where nn = (fromInteger . reflect) (Proxy :: Proxy n)

compMap :: [Int] -> IMap.IntMap Int -> IMap.IntMap Int -> Ordering
compMap js m1 m2 = foldl (compExp m1 m2) EQ js

compExp :: IMap.IntMap Int -> IMap.IntMap Int -> Ordering -> Int -> Ordering
compExp m1 m2 EQ n = compare e1 e2
    where e1 = fromMaybe 0 (m1 IMap.!? n)
          e2 = fromMaybe 0 (m2 IMap.!? n)
compExp _ _ a _ = a
--}

instance Ord (Mon n RP.Lex) where
    compare = lexCompare

instance Ord (Mon n RP.GLex) where
    a `compare` b = let aVb = (totalDegree a) `compare` (totalDegree b)
                    in  if aVb == EQ
                        then a `lexCompare` b
                        else aVb

instance Arity n => Ord (Mon n RP.GRevLex) where
    a `compare` b = let aVb = (totalDegree a) `compare` (totalDegree b)
                    in  if aVb == EQ
                        then a `revLexCompare` b
                        else aVb

instance Arity n => Show (Mon n o) where
    show = monListToString . multiDegree

instance Semigroup (Mon n o) where
    a <> b = MakeMon $ IMap.unionWith (+) (degMap a) (degMap b)

instance Monoid (Mon n o) where
    mempty = MakeMon $ IMap.empty

instance Arity n => Readable (Mon n o) where
    fromString = makeMon . monMapFromString nn where
        nn = (fromInteger . reflect) (Proxy :: Proxy n)

-- | Determine if two monomials are relatively prime.
coprime :: Mon n o -> Mon n o -> Bool
a `coprime` b = a `gcdMon` b == mempty

-- | Determines if the first argument divides the second argument
divides :: Mon n o -> Mon n o -> Bool
a `divides` b = all (`varDivides` (degList b)) (degList a) where
    degList         = IMap.assocs . degMap
    varDivides _ [] = False
    varDivides (n1,e1) ((n2,e2):vars)
        | n1 > n2   = varDivides (n1,e1) vars
        | otherwise = n1 == n2 && e1 <= e2

-- | Given monomials b and a, returns a monomial d such that b = ad
divideBy :: Mon n o -> Mon n o -> Maybe (Mon n o)
b `divideBy` a = if a `divides` b then Just (makeMon diff) else Nothing where
    diff = IMap.unionWith (-) (degMap b) (degMap a)

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
multiDegree :: forall n o. Arity n => Mon n o -> [Int]
multiDegree = rpad nn . IMap.foldlWithKey acc [] . degMap where
    acc lst k exp   = rpad (k-1) lst ++ [exp]
    nn              = (fromInteger . reflect) (Proxy :: Proxy n)
    rpad m xs       = take m $ xs ++ repeat 0

-- | The sum of the exponents of the variables in a monomial
totalDegree :: Mon n o -> Int
totalDegree = sum . IMap.elems . degMap
