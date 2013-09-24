-- | Serialization of statically-sized types with the "Data.Binary"
-- library.
module Linear.Binary where
import Control.Applicative
import Data.Binary
import Data.Foldable (Foldable, traverse_)
import Data.Traversable (Traversable, sequenceA)

-- | Serialize a linear type.
putLinear :: (Binary a, Foldable t) => t a -> Put
putLinear = traverse_ put

-- | Deserialize a linear type.
getLinear :: (Binary a, Applicative t, Traversable t) => Get (t a)
getLinear = sequenceA $ pure get
