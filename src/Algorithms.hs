-------------------------------------------------------------------------------
-- |
-- Authors : Tommy Meek and Frank Moore
--
-- A module for polynomial algorithms.
-------------------------------------------------------------------------------
module Algorithms ( longDiv
                  , reduce
                  , (//)
                  , (/%)
                  ) where

import qualified Data.Vector.Fixed as V
import Control.Monad (join, liftM2, liftM3)
import Data.List.Index (modifyAt)
--import Data.Maybe (fromMaybe)
import qualified Coefficient as C
import qualified DenseMonom as M
import qualified Polynomial as P

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
    where Just lth = p ?/ g

-- r := r + LT(p)
rUpdate :: (V.Arity n, Num (Coef r), Num (Poly r n o))
           => Poly r n o -> Poly r n o -> Poly r n o
rUpdate p r = r + ltp
    where Just ltp = P.leadTerm p

-- q := q + LT(p)/LT(g)
qUpdate :: (V.Arity n, Fractional (Coef r), Num (Poly r n o))
           => Poly r n o -> Poly r n o -> [Poly r n o] -> Int -> [Poly r n o]
qUpdate p g qs n = modifyAt n (+ lth) qs
    where Just lth = p ?/ g

-- f ?/ g = LT(f)/LT(g)
(?/) :: (Fractional (Coef r), V.Arity n)
        => Poly r n o -> Poly r n o -> Maybe (Poly r n o)
f ?/ g = liftM3 scaleFrac (P.leadCoef f) (P.leadCoef g) underlap
    where underlap = liftJoin2 M.factor (P.leadMonom f) (P.leadMonom g)
          scaleFrac n d m = (n / d) `P.scaleMon` m

-- Ganked from Control.Monad.HT
liftJoin2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
liftJoin2 f ma mb = join (liftM2 f ma mb)



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
--          Just lmh = lmf `M.factor` lmg

--applyWhen :: (b -> a -> Bool) -> (a -> b -> a) -> (a -> a -> a) -> (a -> a) -> [b] -> (a,a) -> (a,a)
--applyWhen _ _ f1 f2 [] (x1,x2) = (f1 x1 x2, f2 x1)
--applyWhen pred f0 f1 f2 ys (x1,x2) = if (pred (head ys) x1)
--                                     then (f0 x1 (head ys), x2)
--                                     else applyWhen pred f0 f1 f2 (tail ys) (x1,x2)

--head . dropWhile isNothing . map (factor leadMonom p)
