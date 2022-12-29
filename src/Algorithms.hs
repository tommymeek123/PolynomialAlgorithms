-------------------------------------------------------------------------------
-- |
-- Authors : Tommy Meek and Frank Moore
--
-- A module for polynomial algorithms.
-------------------------------------------------------------------------------
module Algorithms ( divides
                  , longDiv
                  , reduce
                  ) where

import qualified Data.Vector.Fixed as V
import qualified Coefficient as C
import qualified DenseMonom as M
import qualified Polynomial as P

-- Type synonyms
type Coef = C.Coefficient
type Mon = M.Monomial
type Poly = P.Polynomial

--Stubbed
longDiv :: (Ord (Mon n o), Num (Coef r), V.Arity n)
            => Poly r n o -> [Poly r n o] -> (Poly r n o, [Poly r n o])
longDiv _ _ = (fromInteger 0, [])

--Stubbed
divides :: (Ord (Mon n o), Num (Coef r), V.Arity n)
            => Poly r n o -> Poly r n o -> Bool
divides _ _ = True




reduce :: (Ord (Mon n o), Fractional (Coef r), V.Arity n)
           => Poly r n o -> [Poly r n o] -> Poly r n o
reduce f gs = snd $ outerLoop gs (f, 0)

outerLoop :: (Ord (Mon n o), Fractional (Coef r), V.Arity n)
              => [Poly r n o] -> (Poly r n o, Poly r n o) -> (Poly r n o, Poly r n o)
outerLoop _ (0, r) = (0, r)
outerLoop gs (p, r) = outerLoop gs $ innerLoop gs (p,r)

innerLoop :: (Ord (Mon n o), Fractional (Coef r), V.Arity n)
              => [Poly r n o] -> (Poly r n o, Poly r n o) -> (Poly r n o, Poly r n o)
innerLoop [] (p,r) = (rUpdate p r, P.dropLeadTerm p)
innerLoop gs (p,r) = if (head gs) `leadMonDivs` p
                     then (pUpdate p (head gs), r)
                     else innerLoop (tail gs) (p,r)

leadMonDivs :: V.Arity n => Poly r n o -> Poly r n o -> Bool
g `leadMonDivs` f = lmg `M.divides` lmf
    where Just lmf = P.leadMonom f
          Just lmg = P.leadMonom g

pUpdate :: (Ord (Mon n o), Fractional (Coef r), V.Arity n)
            => Poly r n o -> Poly r n o -> Poly r n o
pUpdate p g = p - lth * g
    where Just lth = p ?/ g

rUpdate :: (V.Arity n, Num (Coef r), Num (Poly r n o))
            => Poly r n o -> Poly r n o -> Poly r n o
rUpdate p r = r + ltp
    where Just ltp = P.leadTerm p

-- f ?/ g = LT(f)/LT(g)
(?/) :: (Fractional (Coef r), V.Arity n)
         => Poly r n o -> Poly r n o -> Maybe (Poly r n o)
f ?/ g = if lmg `M.divides` lmf
         then Just $ (lcf / lcg) `P.scaleMon` lmh
         else Nothing
    where Just lcf = P.leadCoef f
          Just lcg = P.leadCoef g
          Just lmf = P.leadMonom f
          Just lmg = P.leadMonom g
          Just lmh = lmf `M.factor` lmg

--applyWhen :: (b -> a -> Bool) -> (a -> b -> a) -> (a -> a -> a) -> (a -> a) -> [b] -> (a,a) -> (a,a)
--applyWhen _ _ f1 f2 [] (x1,x2) = (f1 x1 x2, f2 x1)
--applyWhen pred f0 f1 f2 ys (x1,x2) = if (pred (head ys) x1)
--                                     then (f0 x1 (head ys), x2)
--                                     else applyWhen pred f0 f1 f2 (tail ys) (x1,x2)

--head . dropWhile isNothing . map (factor leadMonom p)
