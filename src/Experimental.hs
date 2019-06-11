{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
GeneralizedNewtypeDeriving, DeriveGeneric, DeriveAnyClass, DerivingStrategies,
TypeOperators, StandaloneDeriving #-}
{-|
Module      : Experimental
Description : Analysis of Markov processes with known parameters.
Maintainer  : atloomis@math.arizona.edu
Stability   : experimental

Two type classes for deterministically analyzing
Markov chains with known parameters.
'Markov0' is intended to list possible outcomes.
'Markov' should allow for more sophisticated analysis.
See "Examples" for examples.
See README for a detailed description.
-}
-- module Markov (
              -- -- *Markov
              -- , Markov (..)
              -- -- *MProd
              -- , MProd (..)
              -- -- *MSum
              -- , MSum (..)
              -- -- *MNull
              -- , MNull (..)
              -- ) where
module Experimental where

import Control.Applicative
import Data.Semigroup
import qualified Data.List as DL
import qualified Data.List.NonEmpty as NE
import qualified Data.Functor.Contravariant as FC
import qualified GHC.Float as GF
import Generics.Deriving as GD
import Data.Discrimination as DD


---------------------------------------------------------------------------------------
-- Markov1
---------------------------------------------------------------------------------------

class (Combine t, Combine m, Grouping t, Grouping m, Monoid t) => Markov1 t m where
    transition1 :: m -> [(t, m -> m)]
    step1       :: (t,m) -> [(t,m)]
    chain1      :: [(t,m)] -> [[(t,m)]]
    step1 x = fmap (x <**>) (transition1 $ snd x)
    chain1 = DL.iterate $ map (summarize . NE.fromList)
             . DD.groupWith quotient . concatMap step1
             -- WARNING: DD.group does not currently respect equivalence classes,
             -- hence the use of quotient.

---------------------------------------------------------------------------------------
-- Gouping instances for Double/Float
---------------------------------------------------------------------------------------

instance Grouping Float where grouping = FC.contramap GF.castFloatToWord32 grouping
instance Grouping Double where grouping = FC.contramap GF.castDoubleToWord64 grouping

---------------------------------------------------------------------------------------
-- Combine
---------------------------------------------------------------------------------------

-- |Within equivalence classes, @combine@ should be associative,
-- commutative, and should be idempotent up to equivalence.
-- Quotient should map equivalence classes to representatives.
-- I.e.  if x == y == z,
-- prop> (x `combine y) `combine` z = x `combine` (y `combine` z)
-- prop> x `combine` y = y `combine` x
-- prop> x `combine` x == x
-- prop> quotient x = quotient y
class Combine a where
    combine  :: a -> a -> a
    summarize :: NE.NonEmpty a -> a
    quotient :: a -> a
    summarize (a NE.:| b) = foldl combine a b
    quotient = id
    -- Figure out how to do quotient :: Grouping b => a -> b

instance (Combine a, Combine b) => Combine (a,b) where
    combine (w,x) (y,z) = (combine w y, combine x z)
    quotient (w,x) = (quotient w, quotient x)

instance (Combine a, Combine b, Combine c) => Combine (a,b,c) where
    combine (a,w,x) (b,y,z) = (combine a b, combine w y, combine x z)
    quotient (w,x,y) = (quotient w, quotient x, quotient y)

---------------------------------------------------------------------------------------
-- Easier way to write nested 2-tuples
---------------------------------------------------------------------------------------

type a :* b = (a,b)
(>*<) :: a -> b -> a :* b
a >*< b = (a,b)
infixl 5 >*<

---------------------------------------------------------------------------------------
-- Merge
---------------------------------------------------------------------------------------

-- |Does not combine unless equal,
-- @combine _ = id@.
newtype Merge a = Merge a
    deriving (Eq, Generic)
    deriving newtype (Semigroup, Monoid)
    deriving anyclass Grouping

instance Show a => Show (Merge a) where
    show (Merge a) = show a

instance Combine (Merge a) where
    combine _ = id

---------------------------------------------------------------------------------------
-- Sum
---------------------------------------------------------------------------------------

instance Grouping a => Grouping (Sum a)

instance Combine (Sum a) where
    combine _ = id

---------------------------------------------------------------------------------------
-- Product
---------------------------------------------------------------------------------------

instance Grouping a => Grouping (Product a)
deriving newtype instance Fractional a => Fractional (Product a)

newtype MProd a = MProd {getProd :: Product a}
    deriving Generic
    deriving newtype (Num, Fractional, Semigroup, Monoid)
    deriving anyclass Grouping

instance Eq (MProd a) where
    _ == _ = True

instance Show a => Show (MProd a) where
    show (MProd a) = show a

instance Num a => Combine (MProd a) where
    combine = (+)
    quotient _ = MProd 0

---------------------------------------------------------------------------------------
-- Test
---------------------------------------------------------------------------------------

newtype TestWalk = TW Int
    deriving (Enum, Show, Generic)
    deriving newtype Num
    deriving anyclass Grouping

instance Combine TestWalk where
    combine _ = id

instance Markov1 (Sum Int) TestWalk where
    transition1 _ = [ (0, pred)
                    , (1, succ) ]

instance Markov1 (Sum Int, MProd Double) TestWalk where
    transition1 _ = [ ( (0, MProd $ Product 0.5), pred )
                    , ( (1, MProd $ Product 0.5), succ ) ]

instance Markov1 (Merge String :* Sum Int :* MProd Double) TestWalk where
    transition1 _ = [ ( (Merge "l" >*< 0 >*< 0.5), pred )
                    , ( (Merge "r" >*< 1 >*< 0.5), succ ) ]

