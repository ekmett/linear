{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 710
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Orphans
-----------------------------------------------------------------------------
module Linear.Instances () where

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.Zip
import Data.Complex
import Data.Orphans ()

instance MonadZip Complex where
  mzipWith = liftA2

instance MonadFix Complex where
  mfix f = (let a :+ _ = f a in a) :+ (let _ :+ a = f a in a)
