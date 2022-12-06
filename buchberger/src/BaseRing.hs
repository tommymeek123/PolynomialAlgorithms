module BaseRing ( Coefficient ) where

import Data.Ratio ((%), numerator, denominator)
--import Control.DeepSeq (NFData, rnf)
import qualified RingParams as RP
import PolyParsers (Readable(..), ratFromString, ratToString)

data Coefficient :: RP.Ring -> * where
    Q :: Rational -> Coefficient r
    Fp :: Coefficient r
    deriving Eq

instance Show (Coefficient RP.Q) where
    show (Q r) = ratToString r

--instance NFData Field where
--    rnf (Q a) = rnf a

instance Num (Coefficient RP.Q) where
    (Q r) + (Q s) = Q (r + s)
    (Q r) - (Q s) = Q (r - s)
    (Q r) * (Q s) = Q (r * s)
    abs (Q r)     = Q (abs r)
    signum (Q r)  = Q (signum r)
    fromInteger n = Q $ n % 1

instance Fractional (Coefficient RP.Q) where
    recip (Q r)  = Q (denominator r % numerator r)
    fromRational = Q

instance Readable (Coefficient RP.Q) where
    fromString = Q . ratFromString

instance Show (Coefficient RP.Fp) where
    show _ = show RP.Fp

instance Num (Coefficient RP.Fp) where
    a + b         = Fp
    a - b         = Fp
    a * b         = Fp
    abs a         = Fp
    signum a      = Fp
    fromInteger n = Fp

instance Fractional (Coefficient RP.Fp) where
    recip a        = Fp
    fromRational a = Fp

instance Readable (Coefficient RP.Fp) where
    fromString s = Fp
