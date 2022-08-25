module Polynomial (Poly(..)) where

import qualified Data.Map as Map
import Base
import DenseMonom as Monom

newtype Poly = Poly {monMap :: Map.Map Mon Q} deriving (Eq)

--totalPolyDeg :: Poly -> Int
--maxViewWithKey totalPolyDeg
