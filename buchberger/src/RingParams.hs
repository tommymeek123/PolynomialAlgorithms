module RingParams (Field(..)
                 , MonOrder(..)
                 , RingParams(..)) where

data Field = Q | Fp deriving (Eq,Read,Show)
data MonOrder = Lex | Grlex | Grevlex deriving (Eq,Read,Show)
data RingParams = RingParams { field :: Field
                                , monOrder :: MonOrder
                                , numVars :: Int
                                } deriving (Eq,Read,Show)
