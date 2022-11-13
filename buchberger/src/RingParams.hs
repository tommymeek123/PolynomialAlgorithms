module RingParams ( Field(..)
                  , MonOrder(..)
                  ) where

data Field = Q | Fp deriving (Eq,Read,Show)
data MonOrder = Lex | Glex | GRevLex deriving (Eq,Read,Show)
--data RingParams = RingParams { field :: Field
--                             , order :: MonOrder
--                             , numVars :: Int
--                             } deriving (Eq,Read,Show)

-- Fields
--newtype Q = Q deriving (Eq,Read,Show)
--newtype Fp = Fp deriving (Eq,Read,Show)

-- Monomial Orderings
--newtype Lex = Lex deriving (Eq,Read,Show)
--newtype Grlex = Grlex deriving (Eq,Read,Show)
--newtype Grevlex = Grevlex deriving (Eq,Read,Show)
