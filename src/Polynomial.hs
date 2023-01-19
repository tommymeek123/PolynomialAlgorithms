-----------------------------------------------------------------------------------------
-- |
-- Authors : Tommy Meek and Frank Moore
--
-- A module for commutative polynomials.
-----------------------------------------------------------------------------------------
module Polynomial ( Polynomial
                  , dropLeadTerm
                  , fromMap
                  , isZero
                  , leadCoef
                  , leadMonom
                  , leadMonomAsPoly
                  , leadTerm
                  , leadTermDivs
                  , multiDegree
                  , normalize
                  , numTerms
                  , scale
                  , scaleMon
                  , sPoly
                  , totalDegree
                  , divideByMon
                  , divideByLeadTerm
                  ) where

import Data.Maybe (fromMaybe, isNothing)
--import Control.Monad (liftM)
import Data.Composition ((.:))
import Data.Foldable (maximumBy)
import GHC.TypeLits (Nat)
import qualified Data.Vector.Fixed as V
import qualified Data.Map as Map
import qualified Coefficient as C
import qualified DenseMonom as M
import qualified RingParams as RP
import PolyParsers (Readable(..), polyTupleListFromString, polyListToString)

-- Type synonyms
type Coef = C.Coefficient
type Mon = M.Monomial
type Poly = Polynomial

-- | A commutative polynomial
newtype Polynomial :: RP.Ring -> Nat -> RP.MonOrder -> * where
    MakePoly :: { monMap :: Map.Map (Mon n o) (Coef r) } -> Polynomial r n o

deriving instance V.Arity n => Eq (Poly r n o)

makePoly :: Num (Coef r) => Map.Map (Mon n o) (Coef r) -> Poly r n o
makePoly = MakePoly . Map.filter (/= 0)

instance (Show (Mon n o), Show (Coef r)) => Show (Poly r n o) where
    show = polyListToString
         . map (\(m,c) -> show c ++ show m)
         . reverse
         . Map.assocs
         . monMap

instance (Ord (Mon n o), Readable (Mon n o), Num (Coef r), Readable (Coef r))
         => Readable (Poly r n o) where
    fromString = makePoly
               . Map.fromListWith (+)
               . map (\(ms,cs) -> (fromString ms, fromString cs))
               . polyTupleListFromString

instance (Ord (Mon n o), Num (Coef r), V.Arity n) => Num (Poly r n o) where
    f + g = makePoly $ Map.unionWith (+) (monMap f) (monMap g)
    f * g = Map.foldrWithKey distributeOver 0 (monMap g)
        where distributeOver m c = (+) (leftMultWithCoef m c f)
    abs = id
    signum _ = 1
    fromInteger n = makePoly $ Map.singleton mempty (fromInteger n)
    negate = makePoly . Map.map negate . monMap

-- | Returns a monomial as a single term polynomial.
asPoly :: Num (Coef r) => Mon n o -> Poly r n o
asPoly m = MakePoly $ Map.singleton m 1

-- | Returns the polynomial sans its lead term.
dropLeadTerm :: Poly r n o -> Poly r n o
dropLeadTerm = MakePoly . Map.deleteMax . monMap

-- | Creates a polynomial from a map to Strings representing the coefficients.
fromMap :: (V.Arity n, Num (Coef r), Readable (Coef r), Ord (Mon n o))
           => Map.Map [Int] String -> Poly r n o
fromMap = makePoly . Map.map fromString . Map.mapKeys M.fromList

-- | Returns true on the zero polynomial. False otherwise.
isZero :: Poly r n o -> Bool
isZero = Map.null . monMap

-- | The lead coefficient of a polynomial.
leadCoef :: Poly r n o -> Maybe (Coef r)
leadCoef = fmap snd . Map.lookupMax . monMap

-- | The lead monomial of a polynomial.
leadMonom :: Poly r n o -> Maybe (Mon n o)
leadMonom = fmap fst . Map.lookupMax . monMap

-- | The lead term of a polynomial with the coeficient replaced with 1.
leadMonomAsPoly :: Num (Coef r) => Poly r n o -> Maybe (Poly r n o)
leadMonomAsPoly f = case leadMonom f of
    Just m -> fmap Just makePoly $ Map.singleton m 1
    Nothing -> Nothing

-- | The lead term of a polynomial.
leadTerm :: Num (Coef r) => Poly r n o -> Maybe (Poly r n o)
leadTerm f = (makePoly .: Map.singleton) <$> (leadMonom f) <*> (leadCoef f)

-- | Determines if the lead term of the first argument divides the second.
leadTermDivs :: V.Arity n => Poly r n o -> Poly r n o -> Bool
g `leadTermDivs` f = fromMaybe False $ M.divides <$> leadMonom g <*> leadMonom f

-- Left multiply a polynomial by a monomial.
leftMult :: (Ord (Mon n o), Num (Coef r), V.Arity n)
            => Mon n o -> Poly r n o -> Poly r n o
leftMult m = makePoly . Map.mapKeys (m <>) . monMap

-- Left multiply a polynomial by a monomial and a coeficient
leftMultWithCoef :: (Ord (Mon n o), Num (Coef r), V.Arity n)
                    => Mon n o -> Coef r -> Poly r n o -> Poly r n o
leftMultWithCoef m c = makePoly . Map.map (c *) . Map.mapKeys (m <>) . monMap

-- Lift a binary boolean function to consider Maybes. Depricated. Use aplicative instead.
liftBool :: (a -> b -> Bool) -> Maybe a -> Maybe b -> Bool
liftBool comp ma mb
    | isNothing ma = False
    | isNothing mb = False
    | otherwise = a `comp` b
        where Just a = ma
              Just b = mb

-- | A list of the exponents of the variables in the lead monomial.
multiDegree :: V.Arity n => Poly r n o -> Maybe [Int]
multiDegree = M.multiDegree . leadMonom

-- | Normalizes a polynomial to have a lead coefficient of 1.
normalize :: (Ord (Mon n o), Fractional (Coef r), V.Arity n) => Poly r n o -> Poly r n o
normalize f = fromMaybe 0 $ scale <$> (recip <$> leadCoef f) <*> Just f

-- | The number of nonzero terms in a polynomial.
numTerms :: Poly r n o -> Int
numTerms = Map.size . monMap

-- | Maps a function of monomials over every term in a given polynomial.
pmap :: (Ord (Mon n o), Num (Coef r)) => (Mon n o -> Mon n o) -> Poly r n o -> Poly r n o
pmap f p = makePoly $ Map.mapKeys f (monMap p)

-- | Works like pmap but returns Nothing if any of the terms return Nothing.
pmapM :: (Ord (Mon n o), Num (Coef r), V.Arity n)
         => (Mon n o -> Maybe (Mon n o)) -> Poly r n o -> Maybe (Poly r n o)
pmapM f p = if any (\(m,_) -> isNothing m) factorList
            then Nothing
            else Just
               . makePoly
               . Map.fromList
               . map (\(m,c) -> (fromMaybe mempty m, c)) $ factorList
    where factorList = Map.foldrWithKey (\m c xs -> (f m, c) : xs) [] (monMap p)

-- | Multiplies a polynomial by a scalar value.
scale :: Num (Coef r) => Coef r -> Poly r n o -> Poly r n o
scale c = makePoly . Map.map (c *) . monMap

-- | Multiplies a monomial by a scalar value.
scaleMon :: Num (Coef r) => Coef r -> Mon n o -> Poly r n o
scaleMon c m = makePoly $ Map.singleton m c

-- | The S-polynomial, or overlap relation, of two given polynomials.
sPoly :: (Ord (Mon n o), Fractional (Coef r), V.Arity n)
         => Poly r n o -> Poly r n o -> Maybe (Poly r n o)
sPoly f g = (-) <$> redf <*> redg
    where lcm = M.lcmMon <$> leadMonom f <*> leadMonom g
          fNormalizer = divideByLeadTerm <$> fmap asPoly lcm <*> leadTerm f >>= id
          gNormalizer = divideByLeadTerm <$> fmap asPoly lcm <*> leadTerm g >>= id
          redf = (*) <$> fNormalizer <*> Just f
          redg = (*) <$> gNormalizer <*> Just g

-- | The sum of the exponents of the variables in the lead monomial.
totalDegree :: V.Arity n => Poly r n o -> Maybe Int
totalDegree f | isZero f = Nothing
              | otherwise = (Just
                           . M.totalDegree
                           . maximumBy comp
                           . Map.keys
                           . monMap) f
    where comp a b = compare (M.totalDegree a) (M.totalDegree b)

-- | Divides a polynomial by a monomial
divideByMon :: (Ord (Mon n o), Num (Coef r), V.Arity n)
                  => Poly r n o -> Mon n o -> Maybe (Poly r n o)
f `divideByMon` m = pmapM (`M.divideBy` m) f

-- | Divides a polynomial by the lead term of another polynomial
divideByLeadTerm :: (Ord (Mon n o), Fractional (Coef r), V.Arity n)
                       => Poly r n o -> Poly r n o -> Maybe (Poly r n o)
f `divideByLeadTerm` g = scaleDown <$> (leadCoef g) <*> q
    where scaleDown c = scale (recip c)
          q = leadMonom g >>= (f `divideByMon`)

-- f `divideByLeadTerm` g := LT(f)/LT(g)
--divideByLeadTerm :: (Fractional (Coef r), V.Arity n)
--                       => Poly r n o -> Poly r n o -> Maybe (Poly r n o)
--f `divideByLeadTerm` g = liftM3 scaleFrac (leadCoef f) (leadCoef g) underlap
--    where underlap = liftJoin2 M.divideBy (leadMonom f) (leadMonom g)
--          scaleFrac n d m = (n / d) `scaleMon` m
