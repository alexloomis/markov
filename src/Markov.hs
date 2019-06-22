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
'Markov' should allow for more sophisticated analysis.
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

     -- *Combine
     , Combine (..)
     , Merge (..)
     , Sum (..)
     , Product (..)

     -- *Misc
     , (:*)
     , (>*<)
     , fromLists
     , randomProduct
     , randomPath
     ) where

-- import Configuration.Utils.Operators ((<*<))
import Control.Comonad
import Data.Discrimination (Grouping, grouping)
import Generics.Deriving (Generic)

import Markov.Instance ()

import qualified Control.Monad.Random as MR
import qualified Data.Discrimination as DD
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
class (Applicative t, Comonad t) => Markov t m where
    transition :: m -> [t (m -> m)]
    step       :: t m -> [t m]
    sequential :: [m -> [t (m -> m)]]
    transition = fmap (fmap const) . step . pure
    step x = foldr (concatMap . step') [x] sequential
      where step' f y = (<*> y) <$> f (extract y)
    sequential = [transition]
    {-# MINIMAL transition | step | sequential #-}
    -- Could also be defined as follows:
    --
    -- transition = foldr compose stayPut sequential
      -- where stayPut = const [pure id]
            -- compose g f a = composeWith g a =<< f a
            -- composeWith g a x = (<*< x) <$> g (extract $ fmap ($ a) x)
    -- step x = (<*> x) <$> transition (extract x)
    -- sequential = [fmap (fmap const) . step . pure]

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
    {-# MINIMAL combine | summarize #-}

instance (Combine a, Combine b) => Combine (a,b) where
    combine (w,x) (y,z) = (combine w y, combine x z)

instance (Combine a, Combine b, Combine c) => Combine (a,b,c) where
    combine (a,w,x) (b,y,z) = (combine a b, combine w y, combine x z)

---------------------------------------------------------------------------------------
-- Easier way to write nested 2-tuples
---------------------------------------------------------------------------------------

-- |Easier way to write nested 2-tuples.
type a :* b = (a,b)
-- |Easier way to write nested 2-tuples.
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

-- This causes Data.List.group to act more like Data.Discrimination.group
-- |WARNING! Defined @_ == _ = True@!
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
--
-- prop> all (== length matrix) (map length matrix)
-- prop> length matrix == length states
fromLists :: Eq  b => [[a]] -> [b] -> b -> [(a, c -> b)]
fromLists matrix states b = case DL.elemIndex b states of
    Nothing -> []
    Just n  -> zip (matrix!!n) $ fmap const states
