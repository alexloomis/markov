module Markov.Instance where

import Data.Discrimination (Grouping, grouping)
import Data.Functor.Contravariant (contramap)
import GHC.Float (castFloatToWord32, castDoubleToWord64)

instance Grouping Float where grouping = contramap castFloatToWord32 grouping
instance Grouping Double where grouping = contramap castDoubleToWord64 grouping
