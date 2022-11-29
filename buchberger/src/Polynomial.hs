module Polynomial ( Polynomial(..)
                  , isZero
                  , leadCoef
                  , leadMonom
                  , leadMonomP
                  , leadTerm
--                  , totalDegree
                  ) where

import Data.List (intercalate)
import Data.Maybe (isNothing)
import Control.Monad (liftM2)
import Data.Composition ((.:))
import Data.Foldable (maximumBy)
import GHC.TypeLits (Symbol, Nat, KnownNat)
import qualified Data.Map as Map
import qualified BaseRing as R
import qualified DenseMonom as M
import qualified RingParams as RP
import PolyParsers (Readable(..), polyTupleListFromString)

type C = R.Coefficient
type Mon = M.Monomial
type Poly = Polynomial

data Polynomial :: RP.Ring -> RP.MonOrder -> Nat -> * where
    MakePoly :: { monMap :: Map.Map (Mon o n) (C r) } -> Polynomial r o n

instance (Show (Mon o n), Show (C r)) => Show (Poly r o n) where
    show = let removeOnes s = if head s == '1' then tail s else s
           in intercalate " + "
            . map removeOnes
            . map (\(m,c) -> show c ++ show m)
            . Map.assocs
            . monMap

instance (Ord (Mon o n), Readable (Mon o n), Readable (C r))
          => Readable (Poly r o n) where
    fromString = MakePoly . mapFromString

mapFromString :: (Ord (Mon o n), Readable (Mon o n), Readable (C r))
                => String -> Map.Map (Mon o n) (C r)
mapFromString = Map.fromList
                . map (\(ms,cs) -> (fromString ms, fromString cs))
                . polyTupleListFromString

isZero :: Poly r o n -> Bool
isZero = Map.null . monMap

leadMonom :: Poly r o n -> Maybe (Mon o n)
leadMonom = fmap fst . Map.lookupMax . monMap

leadMonomP :: Num (C r) => Poly r o n -> Maybe (Poly r o n)
leadMonomP p = case leadMonom p of
    Just m -> fmap Just MakePoly $ Map.singleton m (fromInteger 1)
    Nothing -> Nothing

leadCoef :: Poly r o n -> Maybe (C r)
leadCoef = fmap snd . Map.lookupMax . monMap

leadTerm :: Poly r o n -> Maybe (Poly r o n)
leadTerm p = liftM2 (MakePoly .: Map.singleton) (leadMonom p) (leadCoef p)

--totalDegree :: Poly -> Maybe Int
--totalDegree p = M.totalDeg . fst $ maximumBy f (monMap p)
--    where f x y = compare ((M.totalDeg . fst) x) ((M.totalDeg . fst) y)

--totalDegree :: Poly k -> Maybe Int
--totalDegree p = if isZero p
--                then Nothing
--                else Just $ M.totalDeg . maximumBy f . Map.keys . monMap $ p
--                where f x y = compare (M.totalDeg x) (M.totalDeg y)
--
--add :: Poly k -> Poly k -> Poly k
--add p q = Poly $ Map.unionWith (+) (monMap p) (monMap q)

--mult :: Poly -> Poly -> Poly

--leadTerm :: Poly -> Poly
--leadTerm p
--    | Map.null (monMap p) = Poly Map.empty
--    | otherwise = (Poly . uncurry Map.singleton . Map.findMax . monMap) p

--totalPolyDegree :: Poly -> Maybe Int
--totalPolyDegree p
--    | Map.null (monMap p) = Nothing
--    | otherwise = Just $ (totalMonDeg . head . Map.keys . monMap . leadTerm) p

--polyAdd :: Poly -> Poly -> Poly
-- polyAdd = Map.unionWith (+)
