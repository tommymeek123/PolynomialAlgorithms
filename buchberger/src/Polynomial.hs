module Polynomial ( Polynomial(..)
                  , isZero
                  , leadCoef
                  , leadMonom
                  , leadMonomP
                  , leadTerm
                  , totalDegree
                  , multiDegree
                  ) where

import Data.Maybe (isNothing)
import Control.Monad (liftM2)
import Data.Composition ((.:))
import Data.Foldable (maximumBy)
import GHC.TypeLits (Nat)
import qualified Data.Vector.Fixed as V
import qualified Data.Map as Map
import qualified BaseRing as R
import qualified DenseMonom as M
import qualified RingParams as RP
import PolyParsers (Readable(..), polyTupleListFromString, polyListToString)

type Coef = R.Coefficient
type Mon = M.Monomial
type Poly = Polynomial

newtype Polynomial :: RP.Ring -> RP.MonOrder -> Nat -> * where
    MakePoly :: { monMap :: Map.Map (Mon o n) (Coef r) } -> Polynomial r o n

instance (Show (Mon o n), Show (Coef r)) => Show (Poly r o n) where
    show = polyListToString
         . map (\(m,c) -> show c ++ show m)
         . reverse
         . Map.assocs
         . monMap

instance (Ord (Mon o n), Readable (Mon o n), Num (Coef r), Readable (Coef r))
          => Readable (Poly r o n) where
    fromString = MakePoly
               . Map.filter (/= (fromInteger 0))
               . Map.fromListWith (+)
               . map (\(ms,cs) -> (fromString ms, fromString cs))
               . polyTupleListFromString

instance (Ord (Mon o n), Num (Coef r)) => Num (Poly r o n) where
    f + g = MakePoly $ Map.unionWith (+) (monMap f) (monMap g)
--    (Q r) - (Q s) = Q (r - s)
--    (Q r) * (Q s) = Q (r * s)
--    abs (Q r)     = Q (abs r)
--    signum (Q r)  = Q (signum r)
--    fromInteger n = Q $ n % 1
    negate = MakePoly . Map.map negate . monMap

isZero :: Poly r o n -> Bool
isZero = Map.null . monMap

leadMonom :: Poly r o n -> Maybe (Mon o n)
leadMonom = fmap fst . Map.lookupMax . monMap

leadMonomP :: Num (Coef r) => Poly r o n -> Maybe (Poly r o n)
leadMonomP p = case leadMonom p of
    Just m -> fmap Just MakePoly $ Map.singleton m (fromInteger 1)
    Nothing -> Nothing

leadCoef :: Poly r o n -> Maybe (Coef r)
leadCoef = fmap snd . Map.lookupMax . monMap

leadTerm :: Poly r o n -> Maybe (Poly r o n)
leadTerm p = liftM2 (MakePoly .: Map.singleton) (leadMonom p) (leadCoef p)

totalDegree :: V.Arity n => Poly r o n -> Maybe Int
totalDegree p | isZero p = Nothing
              | otherwise = (Just . M.totalDegree . maximumBy comp . Map.keys . monMap) p
    where comp a b = compare (M.totalDegree a) (M.totalDegree b)

multiDegree :: V.Arity n => Poly r o n -> Maybe [Int]
multiDegree = M.multiDegree . leadMonom
