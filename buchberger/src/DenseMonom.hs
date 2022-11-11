module DenseMonom ( Monomial(..)
                  , fromString
                  , totalDeg
                  ) where

import Data.List (mapAccumL, sort)
import Data.List.Split (splitOn)
import Data.Char (digitToInt, isSpace)
import GHC.TypeLits (Symbol, Nat, KnownNat)
import Data.Proxy (Proxy(..))
import Data.Reflection (reflect)
import qualified RingParams as RP

newtype Monomial :: RP.MonOrder -> Nat -> * where
    Monomial :: { degList :: [Int] } -> Monomial o n deriving Eq

instance Ord (Monomial RP.Lex n) where
    compare a b = compare (degList a) (degList b)

instance Ord (Monomial RP.Glex n) where
    compare a b = let aVb = compare (totalDeg a) (totalDeg b)
                  in  if aVb == EQ
                      then compare (degList a) (degList b)
                      else aVb

instance Ord (Monomial RP.GRevLex n) where
    compare a b = let aVb = compare (totalDeg a) (totalDeg b)
                      degDiff = zipWith (-) (degList a) (degList b)
                  in  if aVb == EQ
                      then compare 0
                         . headOrZero
                         . dropWhile (==0)
                         . reverse $ degDiff
                      else aVb

instance Show (Monomial o n) where
    show m = concat . snd $ mapAccumL f 1 (degList m)
        where f n x | x == 0 = (n+1, "")
                    | x == 1 = (n+1, "x_" ++ show n)
                    | otherwise = (n+1, "x_" ++ show n ++ "^" ++ show x)

instance Semigroup (Monomial o n) where
    a <> b = Monomial { degList=zipWith (+) (degList a) (degList b) }

instance (KnownNat n) => Monoid (Monomial o n) where
    mempty = Monomial $ take nn (repeat 0)
        where nn = (fromInteger . reflect) (Proxy :: Proxy n)

fromString :: forall o n. (KnownNat n) => String -> Monomial o n
fromString s = Monomial { degList=listFromString nn s }
    where nn = (fromInteger . reflect) (Proxy :: Proxy n)

listFromString :: Int -> String -> [Int]
listFromString n = rpad n
                 . foldl f []
                 . sort
                 . splitOn "x_"
                 . filter (not . isSpace)
    where f acc s | s == "" = acc
                  | length acc + 1 < (read . head . splitOn "^") s = f (acc ++ [0]) s
                  | '^' `notElem` s = acc ++ [1]
                  | otherwise = acc ++ [(read . last . splitOn "^") s]

totalDeg :: Monomial o n -> Int
totalDeg = sum . degList

headOrZero :: [Int] -> Int
headOrZero [] = 0
headOrZero xs = head xs

rpad :: Int -> [Int] -> [Int]
rpad m xs = take m $ xs ++ repeat 0
