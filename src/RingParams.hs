-------------------------------------------------------------------------------
-- |
-- Authors : Tommy Meek and Frank Moore
--
-- Simple types indicating the parameters of our polynomial ring
-------------------------------------------------------------------------------
module RingParams ( Ring
                  , MonOrder
                  ) where

-- | The base ring used as coefficients for polynomials
data Ring = Q | Fp deriving (Eq,Read,Show)

-- | The Monomial ordering used in our polynomial ring
data MonOrder = Lex | Glex | GRevLex deriving (Eq,Read,Show)
