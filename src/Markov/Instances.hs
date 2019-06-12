module Markov.Instances where

import Data.Discrimination (Grouping, grouping)
import Data.Functor.Contravariant (contramap)
import Data.Functor.Contravariant.Divisible (conquer)
import GHC.Float (castFloatToWord32, castDoubleToWord64)

instance Grouping Float where grouping = contramap castFloatToWord32 grouping
instance Grouping Double where grouping = contramap castDoubleToWord64 grouping
-- |This should be imported, not sure why it isn't.
instance Grouping () where grouping = conquer
