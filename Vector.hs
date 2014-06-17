{-# LANGUAGE DataKinds, GADTs, TypeOperators, KindSignatures,
  ScopedTypeVariables, DataKinds #-}

import GHC.TypeLits
import Data.Proxy


data Vector (n::Nat) a where
    Nil :: Vector 0 a
    Cons :: a -> Vector n a -> Vector (n+1) a


instance Eq a => Eq (Vector n a) where
    Nil == Nil    = True
    Nil == Cons{} = False
    Cons{} == Nil = False
    a == b        = eq a b
        where
          eq :: Eq a => Vector n1 a -> Vector n2 a -> Bool
          eq Nil Nil    = True
          eq Cons{} Nil = False
          eq Nil Cons{} = False
          eq (Cons a as) (Cons b bs) = a==b && eq as bs



instance Show a => Show (Vector n a) where
    show (Cons x v) = "["++show x++" "++show v++"]"
    show Nil = "[]"


v :: Vector 0 ()
v = Nil

v1 :: Vector 1 Int
v1 = Cons 42 Nil

v2 :: Vector 2 Int
v2 = Cons 42 (Cons 123 Nil)


data Vector1 (n::Nat) a where
    Vector1 :: Proxy n -> [a] -> Vector1 n a

instance (KnownNat n, Show a) => Show (Vector1 n a) where
    show (Vector1 p xs) = show (natVal p) ++ ":" ++ show xs


v3 :: Vector1 2 Int
v3 = Vector1 (Proxy::Proxy 2) [1,2]

