{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Linear.Dim where

import Control.Lens
import Data.Complex
#if __GLASGOW_HASKELL__ >= 706
import Language.Haskell.TH hiding (Arity)
#else
import Language.Haskell.TH
#endif

-- When the Nat type in GHC.TypeLits has a working solver, switch to that!
data Nat = Z | S Nat

nat :: Int -> TypeQ
nat n = case compare n 0 of
  LT -> error "negative nat"
  EQ -> conT 'Z
  GT -> conT 'S `appT` nat (n - 1)

type family Fn (n :: Nat) (a :: *) (b :: *) :: *
type instance Fn Z     a b = b
type instance Fn (S n) a b = a -> Fn n a b

type family Dim (v :: * -> *) :: Nat

newtype Fun n a b = Fun { runFun :: Fn n a b }

class Arity (n :: Nat) where
  accum :: (forall m. t (S m) -> a -> t m) -> (t Z -> b) -> t n -> Fn n a b
  apply :: (forall m. t (S m) -> (a, t m)) -> t n -> Fn n a b -> b

instance Arity Z where
  accum f g t = g t
  apply f t h = h

instance Arity n => Arity (S n) where
  accum f g t = \a -> accum f g (f t a)
  apply f t h = case f t of
    (a, u) -> apply f u (h a)

class Arity (Dim v) => Vector v a where
  construct :: Fun (Dim v) a (v a)
  inspect :: v a -> Fun (Dim v) a b -> b

type instance Dim Complex = S (S Z)

-- instance RealFloat a => Vector Complex a where
instance Vector Complex a where
  construct = Fun (:+)
  inspect (x :+ y) (Fun f) = f x y

data Replicate (n :: Nat) = Replicate

replicateF :: forall n a b. Arity n => a -> Fun n a b -> b
replicateF x (Fun h) = apply (\Replicate -> (x, Replicate)) (Replicate :: Replicate n) h

vpure :: Vector v a => a -> v a
vpure x = construct % replicateF x
{-# INLINE vpure #-}

newtype Foldl b n = Foldl b

foldlF :: forall n a b. Arity n => (b -> a -> b) -> b -> Fun n a b
foldlF f b = Fun $ accum (\(Foldl b) a -> Foldl (f b a)) (\(Foldl b) -> b) (Foldl b :: Foldl b n)
{-# INLINE foldlF #-}

vfoldl :: Vector v a => (b -> a -> b) -> b -> v a -> b
vfoldl f z v = foldlF f z % inspect v
{-# INLINE vfoldl #-}

newtype Map b c n = Map (Fn n b c)

mapF :: forall n a b c. Arity n => (a -> b) -> Fun n b c -> Fun n a c
mapF f (Fun h) = Fun $ accum (\(Map h) a -> Map (h (f a)))
                             (\(Map h) -> h)
                             (Map h :: Map b c n)
{-# INLINE mapF #-}

vmap :: (Vector v a, Vector v b) => (a -> b) -> v a -> v b
vmap f v = construct % mapF f % inspect v
{-# INLINE vmap #-}

-- newtype ZipWith a b c n

{-
zipWith :: (Vector v a, Vector v b, Vector v c)
/bin/bash: :w: command not found
zipWith f v w = inspect w
              $ inspect v
              $ zipWithF f
              $ construct
-}
