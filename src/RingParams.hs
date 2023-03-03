-----------------------------------------------------------------------------------------
-- |
-- Authors : Tommy Meek and Frank Moore
--
-- Simple types indicating the parameters of our polynomial ring
-----------------------------------------------------------------------------------------
module RingParams ( Ring(..)
                  , MonOrder(..)
                  ) where

-- | The base ring used as coefficients for polynomials
data Ring = Q | Zero | FTwo | FThree | F Int deriving (Eq,Read,Show)

-- | The Monomial ordering used in our polynomial ring
data MonOrder = Lex | GLex | GRevLex deriving (Eq,Read,Show)


--data family PrimeField (n :: Natural) :: Ring where
--    PrimeField n =
