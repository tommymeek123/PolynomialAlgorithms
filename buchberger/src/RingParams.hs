module RingParams ( Field(..)
                  , MonOrder(..)
                  , NumVars(..)
                  , RingParams(..) where

import Data.Ratio

ModCong

data Field = Q Rational | Fp Int Int deriving (Eq,Ord,Num,Fractional)
data MonOrder = Lex | Grlex | Grevlex deriving (Eq,Show)
type NumVars = Int
newtype RingParams = RingParams { field :: Field
                                , monOrder :: MonOrder
                                , numVars :: NumVars
                                } deriving (Eq,Read,Show)

F5 = Fp 5
