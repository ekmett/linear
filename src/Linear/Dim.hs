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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Linear.Dim where

import Control.Lens
import Data.Complex
import Language.Haskell.TH hiding (Arity)

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
  accum _ g t = g t
  apply _ _ h = h

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

newtype Foldl b (n :: Nat) = Foldl b

foldlF :: forall n a b. Arity n => (b -> a -> b) -> b -> Fun n a b
foldlF f b0 = Fun $ accum (\(Foldl b) a -> Foldl (f b a))
                          (\(Foldl b) -> b)
                          (Foldl b0 :: Foldl b n)
{-# INLINE foldlF #-}

vfoldl :: Vector v a => (b -> a -> b) -> b -> v a -> b
vfoldl f z v = foldlF f z % inspect v
{-# INLINE vfoldl #-}

newtype Map b c n = Map (Fn n b c)

mapF :: forall n a b c. Arity n => (a -> b) -> Fun n b c -> Fun n a c
mapF f (Fun h0) = Fun $ accum (\(Map h) a -> Map (h (f a)))
                              (\(Map h) -> h)
                              (Map h0 :: Map b c n)
{-# INLINE mapF #-}

vmap :: (Vector v a, Vector v b) => (a -> b) -> v a -> v b
vmap f v = construct % mapF f % inspect v
{-# INLINE vmap #-}

newtype Tagn a (n::Nat) = Tagn a
data Tup a c d (n::Nat) = Tup !a (Fn n c d)

type DList a = [a] -> [a]
toList :: DList a -> [a]
toList = ($ [])
snoc :: DList a -> a -> DList a
snoc xs x = xs . (x:)

vzipWithF :: forall n a b c d. Arity n => 
             (a -> b -> c) -> Fun n c d -> Fun n a (Fun n b d)
vzipWithF f (Fun k) = Fun $ 
                      accum (\(Tagn as) !a -> Tagn (as `snoc` a))
                            (\(Tagn as) -> (Fun :: Fn n b d -> Fun n b d) $ 
                                           accum (\(Tup (a:as') h) b -> 
                                                    Tup as' (h (f a b)))
                                                 (\(Tup _ h) -> h)
                                                 (Tup (toList as) k
                                                    :: Tup [a] c d n))
                            (Tagn id :: Tagn (DList a) n)
{-# INLINE vzipWithF #-}

vzipWith :: (Vector v a, Vector v b, Vector v c)
        => (a -> b -> c) -> v a -> v b -> v c
vzipWith f v w = inspect w
               $ inspect v
               $ vzipWithF f
               $ construct
{-# INLINE vzipWith #-}

infixl 6 ^+^, ^-^
infixl 7 ^*, *^, ^/

-- | Compute the sum of two vectors
(^+^) :: (Num a, Vector v a) => v a -> v a -> v a
(^+^) = vzipWith (+)
{-# INLINE (^+^) #-}

-- | Compute the difference between two vectors
(^-^) :: (Num a, Vector v a) => v a -> v a -> v a
(^-^) = vzipWith (-)
{-# INLINE (^-^) #-}

-- | Compute the negation of a vector
gnegate :: (Num a, Vector v a) => v a -> v a
gnegate = vmap negate
{-# INLINE gnegate #-}

-- | Compute the left scalar product
(*^) :: (Num a, Vector v a) => a -> v a -> v a
(*^) a = vmap (a*)
{-# INLINE (*^) #-}

-- | Compute the right scalar product
(^*) :: (Vector v a, Num a) => v a -> a -> v a
f ^* a = vmap (*a) f
{-# INLINE (^*) #-}

-- | Compute division by a scalar on the right.
(^/) :: (Vector v a, Fractional a) => v a -> a -> v a
f ^/ a = vmap (/a) f
{-# INLINE (^/) #-}

-- | Linearly interpolate between two vectors.
lerp :: (Vector v a, Num a) => a -> v a -> v a -> v a
lerp alpha u v = alpha *^ u ^+^ (1 - alpha) *^ v
{-# INLINE lerp #-}

-- | Compute the inner product of two vectors or (equivalently)
-- convert a vector @f a@ into a covector @f a -> a@.
dot :: (Num a, Vector v a) => v a -> v a -> a
dot v w = vfoldl (+) 0 $ vzipWith (*) v w
{-# INLINE dot #-}
