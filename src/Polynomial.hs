-------------------------------------------------------------------------------
-- |
-- Authors : Tommy Meek and Frank Moore
--
-- A module for commutative polynomials.
-------------------------------------------------------------------------------
module Polynomial ( Polynomial
                  , isZero
                  , leadCoef
                  , leadMonom
                  , leadMonomP
                  , leadTerm
                  , multiDegree
                  , totalDegree
                  ) where

import Data.Maybe (isNothing)
import Control.Monad (liftM2)
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
makePoly = MakePoly . Map.filter (/= (fromInteger 0))

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
    f * g = Map.foldrWithKey distrOverF (fromInteger 0) (monMap g)
        where distrOverF m c = (+) (leftMult m c f)
    abs = id
    signum _ = fromInteger 1
    fromInteger n = makePoly $ Map.singleton mempty (fromInteger n)
    negate = makePoly . Map.map negate . monMap

leftMult :: (Ord (Mon n o), Num (Coef r), V.Arity n)
             => Mon n o -> Coef r -> Poly r n o -> Poly r n o
leftMult m c = makePoly . Map.map (c *) . Map.mapKeys (m <>) . monMap

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
leadMonomP :: Num (Coef r) => Poly r n o -> Maybe (Poly r n o)
leadMonomP f = case leadMonom f of
    Just m -> fmap Just makePoly $ Map.singleton m (fromInteger 1)
    Nothing -> Nothing

-- | The lead term of a polynomial.
leadTerm :: Num (Coef r) => Poly r n o -> Maybe (Poly r n o)
leadTerm f = liftM2 (makePoly .: Map.singleton) (leadMonom f) (leadCoef f)

-- | A list of the exponents of the variables in the lead monomial.
multiDegree :: V.Arity n => Poly r n o -> Maybe [Int]
multiDegree = M.multiDegree . leadMonom

-- | The sum of the exponents of the variables in the lead monomial.
totalDegree :: V.Arity n => Poly r n o -> Maybe Int
totalDegree f | isZero f = Nothing
              | otherwise = (Just
                           . M.totalDegree
                           . maximumBy comp
                           . Map.keys
                           . monMap) f
    where comp a b = compare (M.totalDegree a) (M.totalDegree b)