{-# LANGUAGE DataKinds, GADTs, TypeOperators, KindSignatures,
  ScopedTypeVariables, DataKinds #-}

import GHC.TypeLits
import Data.Proxy

data Vector (n::Nat) a where
    Nil :: Vector 0 a
    Cons :: a -> Vector n a -> Vector (n+1) a

instance Show a => Show (Vector n a) where
    show (Cons x v) = "["++show x++" "++show v++"]"
    show Nil = "[]"


v :: Vector 0 ()
v = Nil

v1 :: Vector 1 Int
v1 = Cons 42 Nil



data Vector1 (n::Nat) a where
    Vector1 :: Proxy n -> [a] -> Vector1 n a

instance (KnownNat n, Show a) => Show (Vector1 n a) where
    show (Vector1 p xs) = show (natVal p) ++ ":" ++ show xs


v3 :: Vector1 2 Int
v3 = Vector1 (Proxy::Proxy 2) [1,2]

