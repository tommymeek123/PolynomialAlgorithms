module RingParams ( Ring(..)
                  , MonOrder(..)
                  ) where

data Ring = Q | Fp deriving (Eq,Read,Show)
data MonOrder = Lex | Glex | GRevLex deriving (Eq,Read,Show)
