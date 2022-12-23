-------------------------------------------------------------------------------
-- |
-- Authors : Tommy Meek and Frank Moore
--
-- A module for polynomial algorithms.
-------------------------------------------------------------------------------
module Algorithms ( longDiv
                  , divides
                  , (//)
                  ) where

import qualified Data.Vector.Fixed as V
import qualified Coefficient as C
import qualified DenseMonom as M
import qualified Polynomial as P

-- Type synonyms
type Coef = C.Coefficient
type Mon = M.Monomial
type Poly = P.Polynomial

longDiv :: (Ord (Mon n o), Num (Coef r), V.Arity n)
            => Poly r n o -> [Poly r n o] -> (Poly r n o, [Poly r n o])
longDiv _ _ = (fromInteger 0, [])

divides :: Poly r n o -> Poly r n o -> Bool
divides _ _ = True

(//) :: (Ord (Mon n o), Num (Coef r), V.Arity n)
        => Poly r n o -> [Poly r n o] -> Poly r n o
_ // _ = fromInteger 0
