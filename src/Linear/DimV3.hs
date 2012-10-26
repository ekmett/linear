{-# LANGUAGE KindSignatures, TypeFamilies, DataKinds, MultiParamTypeClasses, 
             FlexibleContexts, ScopedTypeVariables, UndecidableInstances #-}
module Linear.DimV3 (V3, mkV3, module Linear.Dim) where
import Linear.Dim
import Foreign.Storable
import Control.Applicative
import Foreign.Ptr (castPtr)

data family V3 (a :: *) :: *
type instance Dim V3 = S (S (S Z))
data instance V3 Int = V3i {-# UNPACK #-} !Int
                           {-# UNPACK #-} !Int
                           {-# UNPACK #-} !Int
data instance V3 Float = V3f {-# UNPACK #-} !Float
                             {-# UNPACK #-} !Float
                             {-# UNPACK #-} !Float
data instance V3 Double = V3d {-# UNPACK #-} !Double
                              {-# UNPACK #-} !Double
                              {-# UNPACK #-} !Double

instance Vector V3 Int where
  construct = Fun V3i
  inspect (V3i x y z) (Fun f) = f x y z

instance Vector V3 Float where
  construct = Fun V3f
  inspect (V3f x y z) (Fun f) = f x y z

instance Vector V3 Double where
  construct = Fun V3d
  inspect (V3d x y z) (Fun f) = f x y z

mkV3 :: forall a. Vector V3 a => a -> a -> a -> V3 a
mkV3 = let Fun c = construct :: Fun (Dim V3) a (V3 a) in c

instance forall a. (Storable a, Vector V3 a) => Storable (V3 a) where
  sizeOf _ = sizeOf (undefined::a) * 3
  alignment _ = alignment (undefined::a)
  peek ptr = let Fun c = construct::Fun (Dim V3) a (V3 a) 
             in c <$> peek ptr' <*> peekElemOff ptr' 1 <*> peekElemOff ptr' 2
    where ptr' = castPtr ptr
  poke ptr v = inspect v (Fun $ \x y z ->  poke ptr' x >> 
                                           pokeElemOff ptr' 1 y >> 
                                           pokeElemOff ptr' 2 z )
    where ptr' = castPtr ptr

-- We can't write these because of how the R2 class is defined! I need
-- a constraint on the element type to ensure that it is part of the
-- V3 data family, but the element type is not part of the instance
-- head!
-- instance Lin.R2 V3 where
--   _x f v = inspect v (Fun $ \x y z -> (\x' -> runFun construct x' y z) <$> f x)
