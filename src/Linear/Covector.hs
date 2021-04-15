{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Operations on affine spaces.
-----------------------------------------------------------------------------
module Linear.Covector
  ( Covector(..)
  , ($*)
  ) where

import Control.Applicative
import Control.Monad
import Data.Functor.Plus hiding (zero)
import qualified Data.Functor.Plus as Plus
import Data.Functor.Bind
import Data.Functor.Rep as Rep
import Linear.Algebra

-- | Linear functionals from elements of an (infinite) free module to a scalar

newtype Covector r a = Covector { runCovector :: (a -> r) -> r }

infixr 0 $*

($*) :: Representable f => Covector r (Rep f) -> f r -> r
Covector f $* m = f (Rep.index m)

instance Functor (Covector r) where
  fmap f (Covector m) = Covector $ \k -> m (k . f)

instance Apply (Covector r) where
  Covector mf <.> Covector ma = Covector $ \k -> mf $ \f -> ma (k . f)

instance Applicative (Covector r) where
  pure a = Covector $ \k -> k a
  Covector mf <*> Covector ma = Covector $ \k -> mf $ \f -> ma $ k . f

instance Bind (Covector r) where
  Covector m >>- f = Covector $ \k -> m $ \a -> runCovector (f a) k

instance Monad (Covector r) where
#if !(MIN_VERSION_base(4,11,0))
  return a = Covector $ \k -> k a
#endif
  Covector m >>= f = Covector $ \k -> m $ \a -> runCovector (f a) k

instance Num r => Alt (Covector r) where
  Covector m <!> Covector n = Covector $ \k -> m k + n k

instance Num r => Plus (Covector r) where
  zero = Covector (const 0)

instance Num r => Alternative (Covector r) where
  Covector m <|> Covector n = Covector $ \k -> m k + n k
  empty = Covector (const 0)

instance Num r => MonadPlus (Covector r) where
  Covector m `mplus` Covector n = Covector $ \k -> m k + n k
  mzero = Covector (const 0)

instance Coalgebra r m => Num (Covector r m) where
  Covector f + Covector g = Covector $ \k -> f k + g k
  Covector f - Covector g = Covector $ \k -> f k - g k
  Covector f * Covector g = Covector $ \k -> f $ \m -> g $ comult k m
  negate (Covector f) = Covector $ \k -> negate (f k)
  abs _    = error "Covector.abs: undefined"
  signum _ = error "Covector.signum: undefined"
  fromInteger n = Covector $ \ k -> fromInteger n * counital k
