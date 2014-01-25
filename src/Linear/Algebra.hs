{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Linear.Algebra
  ( Algebra(..)
  , Coalgebra(..)
  ) where

import Control.Lens hiding (index)
import Data.Functor.Rep
import Data.Complex
import Data.Void
import Linear.Vector
import Linear.Quaternion
import Linear.Conjugate
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4

-- | An associative unital algebra over a ring
class Num r => Algebra r m where
  mult :: (m -> m -> r) -> m -> r
  unital :: r -> m -> r

instance Num r => Algebra r Void where
  mult _ _ = 0
  unital _ _ = 0

instance Num r => Algebra r (E V0) where
  mult _ _ = 0
  unital _ _ = 0

instance Num r => Algebra r (E V1) where
  mult f _ = f ex ex
  unital r _ = r

instance Num r => Algebra r () where
  mult f () = f () ()
  unital r () = r

instance (Algebra r a, Algebra r b) => Algebra r (a, b) where
  mult f (a,b) = mult (\a1 a2 -> mult (\b1 b2 -> f (a1,b1) (a2,b2)) b) a
  unital r (a,b) = unital r a * unital r b

instance Num r => Algebra r (E Complex) where
  mult f = \ i -> c^.el i where
   c = (f ee ee - f ei ei) :+ (f ee ei + f ei ee)
  unital r i = (r :+ 0)^.el i

instance (Num r, TrivialConjugate r) => Algebra r (E Quaternion) where
  mult f = index $ Quaternion
    (f ee ee - (f ei ei + f ej ej + f ek ek))
    (V3 (f ee ei + f ei ee + f ej ek - f ek ej)
        (f ee ej + f ej ee + f ek ei - f ei ek)
        (f ee ek + f ek ee + f ei ej - f ej ei))
  unital r = index (Quaternion r 0)

-- | A coassociative counital coalgebra over a ring
class Num r => Coalgebra r m where
  comult :: (m -> r) -> m -> m -> r
  counital :: (m -> r) -> r

instance Num r => Coalgebra r Void where
  comult _ _ _ = 0
  counital _ = 0

instance Num r => Coalgebra r () where
  comult f () () = f ()
  counital f = f ()

instance Num r => Coalgebra r (E V0) where
  comult _ _ _ = 0
  counital _ = 0

instance Num r => Coalgebra r (E V1) where
  comult f _ _ = f ex
  counital f = f ex

instance Num r => Coalgebra r (E V2) where
  comult f = index . index v where
    v = V2 (V2 (f ex) 0) (V2 0 (f ey))
  counital f = f ex + f ey

instance Num r => Coalgebra r (E V3) where
  comult f = index . index q where
    q = V3 (V3 (f ex) 0 0)
           (V3 0 (f ey) 0)
           (V3 0 0 (f ez))
  counital f = f ex + f ey + f ez

instance Num r => Coalgebra r (E V4) where
  comult f = index . index v where
    v = V4 (V4 (f ex) 0 0 0) (V4 0 (f ey) 0 0) (V4 0 0 (f ez) 0) (V4 0 0 0 (f ew))
  counital f = f ex + f ey + f ez + f ew

instance Num r => Coalgebra r (E Complex) where
  comult f = \i j -> c^.el i.el j where
    c = ((f ee :+ 0) :+ (0 :+ f ei))
  counital f = f ee + f ei

instance (Num r, TrivialConjugate r) => Coalgebra r (E Quaternion) where
  comult f = index . index
    (Quaternion (Quaternion (f ee) (V3 0 0 0))
            (V3 (Quaternion 0 (V3 (f ei) 0 0))
                (Quaternion 0 (V3 0 (f ej) 0))
                (Quaternion 0 (V3 0 0 (f ek)))))
  counital f = f ee + f ei + f ej + f ek

instance (Coalgebra r m, Coalgebra r n) => Coalgebra r (m, n) where
  comult f (a1, b1) (a2, b2) = comult (\a -> comult (\b -> f (a, b)) b1 b2) a1 a2
  counital k = counital $ \a -> counital $ \b -> k (a,b)
