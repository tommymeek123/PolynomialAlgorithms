module Coefficient ( Coefficient ) where

import Data.Ratio ((%), numerator, denominator)
--import Control.DeepSeq (NFData, rnf)
import qualified RingParams as RP
import PolyParsers (Readable(..), ratFromString, ratToString)

type Q = Coefficient RP.Q
type Fp = Coefficient RP.Fp

data Coefficient :: RP.Ring -> * where
    Q :: Rational -> Coefficient r
    Fp :: Coefficient r
    deriving Eq

instance Show Q where
    show (Q r) = ratToString r

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
    recip (Q r)  = Q (denominator r % numerator r)
    fromRational = Q

instance Readable Q where
    fromString = Q . ratFromString

instance Show Fp where
    show _ = show RP.Fp

instance Num Fp where
    a + b         = Fp
    a - b         = Fp
    a * b         = Fp
    abs a         = Fp
    signum a      = Fp
    fromInteger n = Fp

instance Fractional Fp where
    recip a        = Fp
    fromRational a = Fp

instance Readable Fp where
    fromString s = Fp
