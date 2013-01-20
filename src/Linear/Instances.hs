{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Linear.Instances
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Orphans
-----------------------------------------------------------------------------
module Linear.Instances () where

import Data.HashMap.Lazy as HashMap
import Data.Hashable
import Data.Functor.Bind

instance (Hashable k, Eq k) => Apply (HashMap k) where
  (<.>) = HashMap.intersectionWith id

instance (Hashable k, Eq k) => Bind (HashMap k) where
  -- this is needlessly painful
  m >>- f = HashMap.fromList $ do
    (k, a) <- HashMap.toList m
    case HashMap.lookup k (f a) of
      Just b -> [(k,b)]
      Nothing -> []
