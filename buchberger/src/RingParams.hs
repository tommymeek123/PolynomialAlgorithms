module RingParams ( Q(..)
                  , Fp(..)
                  , Lex(..)
                  , Grlex(..)
                  , Grevlex(..)
                  ) where

--data Field = Q | Fp deriving (Eq,Read,Show)
--data Order = Lex | Grlex | Grevlex deriving (Eq,Read,Show)
--data RingParams = RingParams { field :: Field
--                             , order :: MonOrder
--                             , numVars :: Int
--                             } deriving (Eq,Read,Show)

-- Fields
newtype Q = Q deriving (Eq,Read,Show)
newtype Fp = Fp deriving (Eq,Read,Show)

-- Monomial Orderings
newtype Lex = Lex deriving (Eq,Read,Show)
newtype Grlex = Grlex deriving (Eq,Read,Show)
newtype Grevlex = Grevlex deriving (Eq,Read,Show)
