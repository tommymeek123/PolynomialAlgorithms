module DenseMonom ( Monomial(..)
                  , totalDegree
                  , multiDegree
                  ) where

import GHC.TypeLits (Symbol, Nat, KnownNat)
import Data.Proxy (Proxy(..))
import Data.Reflection (reflect)
import qualified Data.Vector.Fixed as V
import qualified Data.Vector.Fixed.Unboxed as UV
import qualified RingParams as RP
import PolyParsers (Readable(..), monListFromString, monListToString)

newtype Monomial :: RP.MonOrder -> Nat -> * where
    Monomial :: { degVec :: UV.Vec n Int } -> Monomial o n

deriving instance V.Arity n => Eq (Monomial o n)

instance V.Arity n => Ord (Monomial RP.Lex n) where
    compare a b = compare (degVec a) (degVec b)

instance V.Arity n => Ord (Monomial RP.Glex n) where
    compare a b = let aVb = compare (totalDegree a) (totalDegree b)
                  in  if aVb == EQ
                      then compare (degVec a) (degVec b)
                      else aVb

instance V.Arity n => Ord (Monomial RP.GRevLex n) where
    compare a b = let aVb = compare (totalDegree a) (totalDegree b)
                      a' = V.reverse $ degVec a
                      b' = V.reverse $ degVec b
                  in  if aVb == EQ
                      then compare b' a'
                      else aVb

instance V.Arity n => Show (Monomial o n) where
    show m = monListToString (V.toList $ degVec m)

instance V.Arity n => Semigroup (Monomial o n) where
    a <> b = Monomial { degVec = V.zipWith (+) (degVec a) (degVec b) }

instance (KnownNat n, V.Arity n) => Monoid (Monomial o n) where
    mempty = Monomial $ V.fromList' $ take nn (repeat 0)
        where nn = (fromInteger . reflect) (Proxy :: Proxy n)

instance (KnownNat n, V.Arity n) => Readable (Monomial o n) where
    fromString :: forall o n. (KnownNat n, V.Arity n) => String -> Monomial o n
    fromString s = Monomial { degVec = V.fromList' $ monListFromString nn s }
        where nn = (fromInteger . reflect) (Proxy :: Proxy n)

totalDegree :: V.Arity n => Monomial o n -> Int
totalDegree = V.sum . degVec

multiDegree :: V.Arity n => Maybe (Monomial o n) -> Maybe [Int]
multiDegree Nothing = Nothing
multiDegree (Just m) = (Just . V.toList . degVec) m
