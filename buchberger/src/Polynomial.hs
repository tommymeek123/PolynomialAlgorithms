module Polynomial (Poly(..)) where

import qualified Data.Map as Map
import Base
import DenseMonom as Monom
import Debug.Trace

newtype Poly = Poly {monMap :: Map.Map Mon Q} deriving (Eq)

leadTerm :: Poly -> Poly
leadTerm p
    | Map.null (monMap p) = Poly Map.empty
    | otherwise = (Poly . uncurry Map.singleton . Map.findMax . monMap) p

totalPolyDegree :: Poly -> Maybe Int
totalPolyDegree p
    | Map.null (monMap p) = Nothing
    | otherwise = Just $ (totalMonDeg . head . Map.keys . monMap . leadTerm) p

