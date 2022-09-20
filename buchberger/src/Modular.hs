{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Modular where

import Data.Proxy      (Proxy(Proxy))
import Data.Reflection (Reifies, reflect, reify)

data Modulus s = M { getModulus :: Integer }
type Modular s = Data.Reflection.Reifies s Integer

normalize :: forall s. Modular s => Integer -> Modulus s
normalize n = M (mod n modulus) where
   modulus = Data.Reflection.reflect (Data.Proxy.Proxy :: Data.Proxy.Proxy s)

instance Modular s => Num (Modulus s) where
   M a + M b    = normalize (a + b)
   M a * M b    = normalize (a * b)
   abs          = id
   signum (M a) = error "Modular integers do not have signs."
   fromInteger  = normalize
   negate (M a) = normalize (modulus - a) where
      modulus = Data.Reflection.reflect (Data.Proxy.Proxy :: Data.Proxy.Proxy s)

withModulus :: Integer -> (forall s. Modular s => Modulus s) -> Integer
withModulus m v = Data.Reflection.reify m (getModulus . asProxyOf v)
   where
   asProxyOf :: f s -> Proxy s -> f s
   asProxyOf = const

--type FinFieldElt = (forall s. Modular s => Modulus s)
--foo :: FinFieldElt
--foo = 1000 * 1000 * 5 + 2000
