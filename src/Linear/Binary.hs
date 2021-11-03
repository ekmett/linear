-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2013-2015 Edward Kmett and Anthony Cowley
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Serialization of statically-sized types with the "Data.Binary"
-- library.
------------------------------------------------------------------------------
module Linear.Binary
  ( putLinear
  , getLinear
  ) where

import Data.Binary
import Data.Foldable (traverse_)

-- | Serialize a linear type.
putLinear :: (Binary a, Foldable t) => t a -> Put
putLinear = traverse_ put

-- | Deserialize a linear type.
getLinear :: (Binary a, Applicative t, Traversable t) => Get (t a)
getLinear = sequenceA $ pure get
