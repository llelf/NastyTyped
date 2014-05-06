
{-# LANGUAGE TypeFamilies, KindSignatures, GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}


import Data.Type.Equality
import GHC.TypeLits as N

data A
data True'
data False'



type family Not a where
    Not True' = False'
    Not False' = True'


