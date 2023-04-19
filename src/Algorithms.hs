-----------------------------------------------------------------------------------------
-- |
-- Authors : Tommy Meek and Frank Moore
--
-- A module for polynomial algorithms.
-----------------------------------------------------------------------------------------
module Algorithms ( gb
                  , isBasisOf
                  , isGB
                  , longDiv
                  , reduce
                  , (//)
                  , (/%)
                  ) where

import Data.Vector.Fixed (Arity)
import Data.List (delete)
import Data.List.Index (modifyAt)
import Data.Maybe (fromMaybe)
import qualified Polynomial as P

-- Type synonyms
type Coef = P.Coefficient
type Mon = P.Monomial
type Poly = P.Polynomial

-- | Returns a list of quotients and a remainder that result from division.
longDiv :: (Ord (Mon n o), Fractional (Coef r), Arity n)
           => Poly r n o -> [Poly r n o] -> ([Poly r n o], Poly r n o)
longDiv f gs = outerLoop gs (f, replicate (length gs) 0, 0) where
    outerLoop _gs (0, qs, r) = (qs, r)
    outerLoop  gs (p, qs, r) = outerLoop gs $ innerLoop gs (p, qs, r)
    innerLoop  [] (p, qs, r) = (P.dropLeadTerm p, qs, rUpdate p r)
    innerLoop  gs (p, qs, r) = if (head gs) `P.leadTermDivs` p
                               then (pUpdate p (head gs),
                                     qUpdate p (head gs) qs (length qs - length gs), r)
                               else innerLoop (tail gs) (p, qs, r)

-- | Returns the remainder of the first argument upon division by the second.
reduce :: (Ord (Mon n o), Fractional (Coef r), Arity n)
          => Poly r n o -> [Poly r n o] -> Poly r n o
reduce f gs = outerLoop gs (f, 0) where
    outerLoop _gs (0, r) = r
    outerLoop  gs (p, r) = outerLoop gs $ innerLoop gs (p,r)
    innerLoop  [] (p,r)  = (P.dropLeadTerm p, rUpdate p r)
    innerLoop  gs (p,r)  = if (head gs) `P.leadTermDivs` p
                           then (pUpdate p (head gs), r)
                           else innerLoop (tail gs) (p,r)

-- p := p − (LT(p)/LT(g))*g
pUpdate :: (Ord (Mon n o), Fractional (Coef r), Arity n)
           => Poly r n o -> Poly r n o -> Poly r n o
pUpdate p g = p - lth * g where
    Just ltp = P.leadTerm p
    Just lth = ltp `P.divideByLeadTerm` g

-- r := r + LT(p)
rUpdate :: (Arity n, Num (Coef r), Num (Poly r n o))
           => Poly r n o -> Poly r n o -> Poly r n o
rUpdate p r = r + ltp where
    Just ltp = P.leadTerm p

-- q := q + LT(p)/LT(g)
qUpdate :: (Ord (Mon n o), Arity n, Fractional (Coef r))
           => Poly r n o -> Poly r n o -> [Poly r n o] -> Int -> [Poly r n o]
qUpdate p g qs n = modifyAt n (+ lth) qs where
    Just ltp = P.leadTerm p
    Just lth = ltp `P.divideByLeadTerm` g

-- | infix version of longDiv
(//) :: (Ord (Mon n o), Fractional (Coef r), Arity n)
        => Poly r n o -> [Poly r n o] -> ([Poly r n o], Poly r n o)
(//) = longDiv

-- | infix version of reduce
(/%) :: (Ord (Mon n o), Fractional (Coef r), Arity n)
        => Poly r n o -> [Poly r n o] -> Poly r n o
(/%) = reduce

-- | Implementation of Buchburger's algorithm for finding a Groebner basis.
basis :: (Ord (Mon n o), Fractional (Coef r), Arity n) => [Poly r n o] -> [Poly r n o]
basis fs = cycle [(f1,f2) | f1 <- fs, f2 <- fs] fs where
    cycle [] gs = gs
    cycle pairs gs = cycle (tail pairs ++ newPairs) (gs ++ newPoly) where
        (g1,g2)  = head pairs
        r        = fromMaybe 0 (P.sPoly g1 g2) /% gs
        newPoly  = if r /= 0 then [P.normalize r] else []
        newPairs = if r /= 0 then [(r,g) | g <- gs] else []

{--
-- Implementation of Buchburger's using index tuples and optimizations.
basis :: (Ord (Mon n o), Fractional (Coef r), Arity n) => [Poly r n o] -> [Poly r n o]
basis fs = cycle [(m,n) | n <- [0..length fs - 1], m <- [0..n-1]] fs
    where
        cycle [] gs = gs
        cycle pairs gs = cycle (tail pairs ++ newPairs r gs) (gs ++ newPoly)
            where
                g1 = gs !! fst (head pairs)
                g2 = gs !! snd (head pairs)
                r = fromMaybe 0 (P.sPoly g1 g2) /% gs
                newPoly = if r /= 0 then [P.normalize r] else []

-- Determine which new S-pairs to calculate during Buchberger's algorithm.
newPairs :: (Ord (Mon n o), Num (Coef r), Arity n)
            => Poly r n o -> [Poly r n o] -> [(Int,Int)]
newPairs 0 _ = []
newPairs r gs = [(m,length gs) | m <- [0..length gs - 1], (not . coprime) m]
    where coprime m = r `P.leadTermCoprime` (gs !! m)

-- Implementation of Buchburger's using polynomial tuples and optimizations.
basis :: (Ord (Mon n o), Fractional (Coef r), Arity n) => [Poly r n o] -> [Poly r n o]
basis fs = cycle start fs
    where
        triangle (x,y) tps = if (y,x) `elem` tps then tps else (x,y):tps
        start = foldr triangle [] [(f1,f2) | f1 <- fs, f2 <- fs, f1 /= f2]
        cycle [] gs = gs
        cycle pairs gs = cycle (tail pairs ++ newPairs r gs) (gs ++ newPoly)
            where
                (g1,g2) = head pairs
                r = fromMaybe 0 (P.sPoly g1 g2) /% gs
                newPoly = if r /= 0 then [P.normalize r] else []

newPairs :: (Ord (Mon n o), Num (Coef r), Arity n)
            => Poly r n o -> [Poly r n o] -> [(Poly r n o, Poly r n o)]
newPairs 0 _ = []
newPairs r gs = [(r,g) | g <- gs, not (P.leadTermCoprime r g)]
--}

-- | Implementation of Buchburger's algorithm to find a reduced Groebner basis.
gb :: (Ord (Mon n o), Fractional (Coef r), Arity n) => [Poly r n o] -> [Poly r n o]
gb fs = [g /% delete g minBasis | g <- minBasis] where
    gs = basis (map P.normalize fs)
    minBasis = filter (\g -> not (any (`P.leadTermDivs` g) (delete g gs))) gs

-- | Determines if the first set is a basis for the ideal generated by the second set.
isBasisOf :: (Ord (Mon n o), Fractional (Coef r), Arity n)
             => [Poly r n o] -> [Poly r n o] -> Bool
gs `isBasisOf` fs = all (==0) [f /% gs | f <- fs]

-- | Determines if the set of polynomials is a Groebner basis for the ideal it generates.
isGB :: (Ord (Mon n o), Fractional (Coef r), Arity n) => [Poly r n o] -> Bool
isGB gs = all (==0) [fromMaybe 0 (P.sPoly g1 g2) /% gs | g1 <- gs, g2 <- gs, g1 /= g2]
