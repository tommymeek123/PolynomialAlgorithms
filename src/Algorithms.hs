-----------------------------------------------------------------------------------------
-- |
-- Authors : Tommy Meek and Frank Moore
--
-- A module for polynomial algorithms.
-----------------------------------------------------------------------------------------
module Algorithms ( gb
                  , longDiv
                  , reduce
                  , (//)
                  , (/%)
                  ) where

import qualified Data.Vector.Fixed as V
import Data.List.Index (modifyAt)
import Data.Maybe (fromMaybe)
import qualified Coefficient as C
import qualified DenseMonom as M
import qualified Polynomial as P
import Debug.Trace (trace, traceShow)

-- Type synonyms
type Coef = C.Coefficient
type Mon = M.Monomial
type Poly = P.Polynomial

-- | Returns a list of quotients and a remainder that result from division.
longDiv :: (Ord (Mon n o), Fractional (Coef r), V.Arity n)
           => Poly r n o -> [Poly r n o] -> ([Poly r n o], Poly r n o)
longDiv f gs = lastTwo $ outerLoop gs (f, take (length gs) (repeat 0), 0)
    where lastTwo (a, b, c) = (b, c)
          outerLoop _ (0, qs, r) = (0, qs, r)
          outerLoop gs (p, qs, r) = outerLoop gs $ innerLoop gs (p, qs, r)
          innerLoop [] (p, qs, r) = (P.dropLeadTerm p, qs, rUpdate p r)
          innerLoop gs (p, qs, r) = if (head gs) `P.leadTermDivs` p
                                    then (pUpdate p (head gs),
                                          qUpdate p (head gs) qs qi, r)
                                    else innerLoop (tail gs) (p, qs, r)
                                    where qi = (length qs) - (length gs)

-- | Returns the remainder of the first argument upon division by the second.
reduce :: (Ord (Mon n o), Fractional (Coef r), V.Arity n)
          => Poly r n o -> [Poly r n o] -> Poly r n o
reduce f gs = snd $ outerLoop gs (f, 0)
    where outerLoop _ (0, r) = (0, r)
          outerLoop gs (p, r) = outerLoop gs $ innerLoop gs (p,r)
          innerLoop [] (p,r) = (P.dropLeadTerm p, rUpdate p r)
          innerLoop gs (p,r) = if (head gs) `P.leadTermDivs` p
                               then (pUpdate p (head gs), r)
                               else innerLoop (tail gs) (p,r)

-- | infix version of longDiv
(//) :: (Ord (Mon n o), Fractional (Coef r), V.Arity n)
        => Poly r n o -> [Poly r n o] -> ([Poly r n o], Poly r n o)
(//) = longDiv

-- | infix version of reduce
(/%) :: (Ord (Mon n o), Fractional (Coef r), V.Arity n)
        => Poly r n o -> [Poly r n o] -> Poly r n o
(/%) = reduce

-- p := p − (LT(p)/LT(g))*g
pUpdate :: (Ord (Mon n o), Fractional (Coef r), V.Arity n)
           => Poly r n o -> Poly r n o -> Poly r n o
pUpdate p g = p - lth * g
    where Just ltp = P.leadTerm p
          Just lth = ltp `P.divideByLeadTerm` g

-- r := r + LT(p)
rUpdate :: (V.Arity n, Num (Coef r), Num (Poly r n o))
           => Poly r n o -> Poly r n o -> Poly r n o
rUpdate p r = r + ltp
    where Just ltp = P.leadTerm p

-- q := q + LT(p)/LT(g)
qUpdate :: (Ord (Mon n o), V.Arity n, Fractional (Coef r))
           => Poly r n o -> Poly r n o -> [Poly r n o] -> Int -> [Poly r n o]
qUpdate p g qs n = modifyAt n (+ lth) qs
    where Just ltp = P.leadTerm p
          Just lth = ltp `P.divideByLeadTerm` g

-- | Implementation of Buchburger's algorithm to find a Groebner basis.
--gb :: (Ord (Mon n o), Fractional (Coef r), V.Arity n) => [Poly r n o] -> [Poly r n o]
--gb fs = if fs == gs then fs else gb gs
--    where gs = cycle1 (fs, fs)
--          cycle1 ([], gs) = gs
--          cycle1 (fs, gs) = cycle1 ((tail fs), (cycle2 (head fs) (tail fs, gs)))
--          cycle2 f ([], gs) = gs
--          cycle2 f (fs, gs) = cycle2 f (tail fs ++ newPoly, gs ++ newPoly)
--              where r = fromMaybe 0 (P.sPoly f (head fs)) /% gs
--                    newPoly = if r /= 0 then [r] else []

-- | Implementation of Buchburger's algorithm to find a Groebner basis.
gb :: (Ord (Mon n o), Fractional (Coef r), V.Arity n) => [Poly r n o] -> [Poly r n o]
gb fs = if fs == gs then fs else gb gs
    where startingPairs = [(m,n) | n <- [0..length fs - 1], m <- [0..n-1]]
          gs = cycle1 (startingPairs, fs)
          cycle1 ([], gs) = gs
          cycle1 (pairs, gs) = cycle1 (tail pairs ++ newPairs, gs ++ newPoly)
              where g1 = gs !! fst (head pairs)
                    g2 = gs !! snd (head pairs)
                    r = fromMaybe 0 (P.sPoly g1 g2) /% gs
                    newPoly = if r /= 0 then [r] else []
                    newPairs = if r /= 0
                               then [(m,length gs - 1) | m <- [0..length gs - 2]]
                               else []




--g `leadMonDivs` f = lmg `M.divides` lmf
--    where Just lmf = P.leadMonom f
--          Just lmg = P.leadMonom g

--pUpdate p g = fromMaybe $ (p -) <$> ((g *) <$> (p ?/ g))

--rUpdate p r = fromMaybe $ (r +) <$> (P.leadTerm p)

--f ?/ g = if lmg `M.divides` lmf
--         then Just $ (lcf / lcg) `P.scaleMon` lmh
--         else Nothing
--    where Just lcf = P.leadCoef f
--          Just lcg = P.leadCoef g
--          Just lmf = P.leadMonom f
--          Just lmg = P.leadMonom g
--          Just lmh = lmf `M.divideBy` lmg

--applyWhen :: (b -> a -> Bool) -> (a -> b -> a) -> (a -> a -> a) -> (a -> a)
--             -> [b] -> (a,a) -> (a,a)
--applyWhen _ _ f1 f2 [] (x1,x2) = (f1 x1 x2, f2 x1)
--applyWhen pred f0 f1 f2 ys (x1,x2) = if (pred (head ys) x1)
--                                     then (f0 x1 (head ys), x2)
--                                     else applyWhen pred f0 f1 f2 (tail ys) (x1,x2)

--head . dropWhile isNothing . map (divideBy leadMonom p)(
