module BaseRing ( Q(..)
                , Readable(..)
                ) where

import Data.Ratio ((%), numerator, denominator)
--import Control.DeepSeq (NFData, rnf)
import qualified RingParams as RP

-- Class for parsing from a String. Ideally, this should be replaced by Parsec.
class  Readable a  where
    {-# MINIMAL fromString #-}
    fromString :: String -> a

data Q = Q Rational deriving (Eq,Ord)

instance Show Q where
    show (Q r) = if d == 1
                 then show n
                 else show n ++ "/" ++ show d
                 where n = numerator r
                       d = denominator r

--instance NFData Field where
--    rnf (Q a) = rnf a

instance Num Q where
   (Q r) + (Q s) = Q (r + s)
   (Q r) - (Q s) = Q (r - s)
   (Q r) * (Q s) = Q (r * s)
   abs (Q r)     = Q (abs r)
   signum (Q r)  = Q (signum r)
   fromInteger n = Q $ n % 1

instance Fractional Q where
    recip (Q r) = Q (denominator r % numerator r)
    fromRational = Q

instance Readable Q where
    fromString = ratFromString

--fromString :: (Readable k) => RP.RingParams -> String -> k
--fromString r = case RP.field r of RP.Q  -> ratFromString
--                                  RP.Fp -> fpFromString

data Fp = Fp deriving (Eq,Ord,Show)

instance Readable Fp where
    fromString = fpFromString

ratFromString :: String -> Q
ratFromString [] = Q $ 1 % 1
ratFromString xs = if '/' `elem` xs
                   then Q $ n % d
                   else Q $ read xs % 1
                   where n = read $ takeWhile (/= '/') xs
                         d = read . tail $ dropWhile (/= '/') xs

fpFromString :: String -> Fp
fpFromString = \_ -> Fp
