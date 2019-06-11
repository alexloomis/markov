{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
GeneralizedNewtypeDeriving, DeriveGeneric, DeriveAnyClass, DerivingStrategies,
TypeOperators, StandaloneDeriving #-}
{-|
Module      : Markov
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
module Markov (
              -- *Markov0
                Markov0 (..)
              -- *Markov
              , Markov (..)
              -- *Combine
              , Combine (..)
              , Merge (..)
              , Sum (..)
              , Prod (..)
              -- *Misc
              , (:*)
              , (>*<)
              ) where

import Control.Applicative
import Data.Semigroup
import Generics.Deriving
import Data.Discrimination as DD
import qualified Data.List as DL
import qualified Data.List.NonEmpty as NE
import qualified Data.Functor.Contravariant as FC
import qualified GHC.Float as GF

---------------------------------------------------------------
-- Markov0
---------------------------------------------------------------

-- |A basic implementation of Markov chains.
class (Eq m) => Markov0 m where
    -- |The transition functions from a state.
    transition0 :: m -> [m -> m]
    step0       :: m -> [m]
    -- |Iterated steps.
    chain0      :: [m] -> [[m]]
    step0 x = fmap ($ x) (transition0 x)
    chain0  = DL.iterate' $ DL.nub . concatMap step0

---------------------------------------------------------------------------------------
-- Markov
---------------------------------------------------------------------------------------

-- |An implementation of Markov chains.
-- To speed up @chain@, try replacing @groupWith f@
-- in the definition with @group . sort@.
class (Combine t, Grouping t, Grouping m, Monoid t) => Markov t m where
    transition :: m -> [(t, m -> m)]
    step       :: (t,m) -> [(t,m)]
    chain      :: [(t,m)] -> [[(t,m)]]
    step x = fmap (x <**>) (transition $ snd x)
    -- |Iterated steps, with equal states combined using 'summarize' operation.
    chain  = DL.iterate' $ map (summarize' . NE.fromList)
             . DD.groupWith f . concatMap step
             where summarize' xs@((_,b)NE.:|_) = (summarize . fmap fst $ xs, b)
                   f (a,b) = (quotient a, b)
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
-- I.e.  if @x == y == z@,
--
-- prop> (x `combine` y) `combine` z = x `combine` (y `combine` z)
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

-- |Easier way to write nested 2-tuples.
type a :* b = (a,b)
-- |Easier way to write nested 2-tuples,
-- since @a >*\< b >*\< c >*< d@
-- is much easier to read than
-- @(((a,b),c),d)@.
-- Left associative, binds weaker than @+@
-- but stronger than @==@.
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
    deriving newtype (Semigroup, Monoid, Enum, Num, Fractional, Show)
    deriving anyclass Grouping

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

deriving newtype instance Enum a => Enum (Product a)
instance Grouping a => Grouping (Product a)
deriving newtype instance Fractional a => Fractional (Product a)

-- |Does not effect equality of tuple,
-- @combine x y = x + y@.
newtype Prod a = Prod {getProd :: Product a}
    deriving Generic
    deriving newtype (Num, Fractional, Semigroup, Monoid, Enum, Show)
    deriving anyclass Grouping

instance Eq (Prod a) where
    _ == _ = True

instance Num a => Combine (Prod a) where
    combine = (+)
    quotient _ = Prod 0

---------------------------------------------------------------------------------------
-- Test
---------------------------------------------------------------------------------------

newtype TestWalk = TW Int
    deriving (Enum, Show, Generic)
    deriving newtype Num
    deriving anyclass Grouping

instance Combine TestWalk where
    combine _ = id

instance Markov (Sum Int) TestWalk where
    transition _ = [ (0, pred)
                   , (1, succ) ]

instance Markov (Sum Int, Prod Double) TestWalk where
    transition _ = [ ( (0, Prod $ Product 0.5), pred )
                   , ( (1, Prod $ Product 0.5), succ ) ]

instance Markov (Merge String :* Sum Int :* Prod Double) TestWalk where
    transition _ = [ ( (Merge "l" >*< 0 >*< 0.5), pred )
                   , ( (Merge "r" >*< 1 >*< 0.5), succ ) ]

---------------------------------------------------------------------------------------
-- Test
---------------------------------------------------------------------------------------

-- |Randomly choose a 'Prod' by probability.
-- randomProd :: (Real a, MR.MonadRandom m) => [(Prod a, b)] -> m (Prod a, b)
-- randomProd xs = MR.fromList . map (\x -> (x, toRational $ fst x)) $ xs

-- |Returns a single realization of a Markov chain.
-- randomPath :: (Markov (MProd a) b, Real a, MR.RandomGen g) => MProd a b -> g -> [MProd a b]
-- randomPath x g = map (flip MR.evalRand g) . iterate (>>= (randomMProd . step)) $ pure x

-- |Create a transition function from a transition matrix.
-- If [[a]] is an n x n matrix, length [b] should be n.
-- fromLists :: Eq  b => [[a]] -> [b] -> MProd a b -> [MProd a (b -> b)]
-- fromLists matrix states (MProd _ b) = case DL.elemIndex b states of
    -- Nothing -> []
    -- Just n  -> zipWith MProd (matrix!!n) toState
    -- where toState = map (\x -> (\_ -> x)) states
