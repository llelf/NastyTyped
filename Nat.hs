{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeFamilies,
    UndecidableInstances, GADTs #-}
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality
import Data.Proxy

type family Sqrt (n::Nat) :: Nat where
    Sqrt (n::Nat) = Sqrt' n n

type family Sqrt' (n::Nat) (k::Nat) :: Nat where
    Sqrt' (n::Nat) (k::Nat) = If (k^2 == n) k (Sqrt' n (Sub1 k))

sqrt1 :: Sqrt 36 :~: 6
sqrt1 = Refl


-- XXX ???
type family Log2 (n::Nat) :: Nat where
    Log2 (n::Nat) = If (n == 0) 0 (1 + Log2 (Div n 2))

type family Div (n::Nat) (m::Nat) :: Nat where
    Div (n::Nat) (m::Nat) = Div' n m 0

type family Div' (n::Nat) (m::Nat) (k::Nat) :: Nat where
    Div' (n::Nat) (m::Nat) (k::Nat) = If (n+1 <=? m*k)
                                         (k-1)
                                         (Div' n m (If (k==n) n (k+1)))



data Nat' where
    Z :: Nat'
    S :: Nat' -> Nat'


type family ToNat (n::Nat') :: Nat where
    ToNat Z = 0
    ToNat (S n) = 1 + ToNat n

type family FromNat (n::Nat) :: Nat' where
    FromNat n = If (0 == n) Z (S (FromNat (Sub1 n)))

type family Sub1 (n::Nat) :: Nat where
    Sub1 (n::Nat) = If (n==0) 0 (n-1)



