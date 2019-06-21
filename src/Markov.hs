{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{- |
Module      : Markov
Description : Realization of Markov processes with known parameters.
Maintainer  : atloomis@math.arizona.edu
Stability   : experimental

Three type classes for deterministically analyzing
Markov chains with known parameters.
'Markov0' is intended to list possible outcomes,
'Markov' should allow for more sophisticated analysis,
and 'MultiMarkov' is intended to make implementing
hidden Markov models easier.
See "Examples" for examples.
See README for a detailed description.
-}
module Markov (
     -- *Markov0
       Markov0 (..)
     , chain0

     -- *Markov
     , Markov (..)
     , chain
     , randomProduct
     , randomPath

     -- *Combine
     , Combine (..)
     , Merge (..)
     , Sum (..)
     , Product (..)

     -- *Misc
     , (:*)
     , (>*<)
     , fromLists
     ) where

import Configuration.Utils.Operators ((<*<))
import Data.Discrimination (Grouping, grouping)
import Generics.Deriving (Generic)

import Markov.Instances ()

import qualified Control.Monad.Random as MR
import qualified Data.Discrimination as DD
import qualified Data.Foldable as DF
import qualified Data.Functor.Contravariant as FC
import qualified Data.List as DL
import qualified Data.List.NonEmpty as NE

---------------------------------------------------------------
-- Markov0
---------------------------------------------------------------

-- |A basic implementation of Markov chains.
class (Eq m) => Markov0 m where
    transition0 :: m -> [m -> m]
    step0       :: m -> [m]
    -- |Iterated steps.
    transition0 x = const <$> step0 x
    step0 x = ($ x) <$> transition0 x
    {-# MINIMAL transition0 | step0 #-}

chain0 :: Markov0 m => [m] -> [[m]]
chain0 = DL.iterate' $ DL.nub . concatMap step0

---------------------------------------------------------------------------------------
-- Markov
---------------------------------------------------------------------------------------

-- |An implementation of Markov chains.
--
-- prop> foldMap transition $ pure a = transition a
class (Applicative t, Foldable t) => Markov t m where
    transition :: m -> [t (m -> m)]
    step       :: t m -> [t m]
    sequential :: [m -> [t (m -> m)]]
    -- transition = fmap (fmap const) . step . pure
    -- step x = foldr (concatMap . step') [x] sequential
      -- where step' f x = (<*> x) <$> foldMap f x
    -- sequential = [transition]
    step x = (<*> x) <$> foldMap transition x
    sequential = [fmap (fmap const) . step . pure]
    transition = foldr phi stayPut sequential
      where phi g f a = [ x <*< y | y <- f a , x <- foldMap g $ fmap ($ a) y ]
            stayPut = const [pure id]
    {-# MINIMAL transition | step | sequential #-}

-- WARNING: DD.group does not currently respect equivalence classes.
-- |Iterated steps, with equal states combined using 'summarize' operation.
chain :: (Combine (t m), Grouping (t m), Markov t m) => [t m] -> [[t m]]
chain = DL.iterate' $ fmap (summarize . NE.fromList) .  DD.group . concatMap step

---------------------------------------------------------------------------------------
-- Combine
---------------------------------------------------------------------------------------

-- |Within equivalence classes, @combine@ should be associative,
-- commutative, and should be idempotent up to equivalence.
-- I.e.  if @x == y == z@,
--
-- prop> (x `combine` y) `combine` z = x `combine` (y `combine` z)
-- prop> x `combine` y = y `combine` x
-- prop> x `combine` x == x
class Combine a where
    combine  :: a -> a -> a
    summarize :: NE.NonEmpty a -> a
    combine a b = summarize . NE.fromList $ [a,b]
    summarize (a NE.:| b) = foldr combine a b

instance (Combine a, Combine b) => Combine (a,b) where
    combine (w,x) (y,z) = (combine w y, combine x z)

instance (Combine a, Combine b, Combine c) => Combine (a,b,c) where
    combine (a,w,x) (b,y,z) = (combine a b, combine w y, combine x z)

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

-- Does not group to combine unless equal.
-- |Values from a 'Monoid' which have the respective
-- binary operation applied each step,
-- where different values mean states should not be combined.
-- E.g., strings with concatenation.
newtype Merge a = Merge a
    deriving (Eq, Generic)
    deriving newtype (Semigroup, Monoid, Enum, Num, Fractional, Show)
    deriving anyclass Grouping

instance Combine (Merge a) where combine = const

---------------------------------------------------------------------------------------
-- Sum
---------------------------------------------------------------------------------------

-- |Values which are added each step
-- where different values mean states should not be combined.
-- E.g., number of times a red ball is picked from an urn.
newtype Sum a = Sum a
    deriving Generic
    deriving newtype (Eq, Enum, Num, Fractional, Show)
    deriving anyclass Grouping

instance Combine (Sum a) where combine = const

instance Num a => Semigroup (Sum a) where x <> y = x + y

instance Num a => Monoid (Sum a) where mempty = 0

---------------------------------------------------------------------------------------
-- Product
---------------------------------------------------------------------------------------

-- Does not effect equality of tuple,
-- @combine x y = x + y@.
-- |Values which are multiplied each step,
-- and combined additively for equal states.
-- E.g., probabilities.
newtype Product a = Product a
    deriving Generic
    deriving newtype (Num, Fractional, Enum, Show)

instance Grouping (Product a) where
    grouping = FC.contramap (const ()) grouping

-- |This causes Data.List.group to act more like Data.Discrimination.group
instance Eq (Product a) where _ == _ = True

instance Num a => Combine (Product a) where combine = (+)

instance Num a => Semigroup (Product a) where x <> y = x * y

instance Num a => Monoid (Product a) where mempty = 1

---------------------------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------------------------

-- |Randomly choose from a list by probability.
randomProduct :: (Real a, MR.MonadRandom m) => [(a, b)] -> m (a, b)
randomProduct = MR.fromList . fmap (\x -> (x, toRational $ fst x))

-- |Returns a single realization of a Markov chain.
randomPath :: (Markov ((,) a) b, Real a, MR.RandomGen g) => (a,b) -> g -> [(a,b)]
randomPath x g = fmap (`MR.evalRand` g) . iterate (>>= (randomProduct . step)) $ pure x

-- |Create a transition function from a transition matrix.
-- If [[a]] is an n x n matrix, length [b] should be n.
-- fromLists :: Eq  b => [[a]] -> [b] -> b -> [(a, b -> b)]
fromLists matrix states b = case DL.elemIndex b states of
    Nothing -> []
    Just n  -> zip (matrix!!n) $ fmap const states
