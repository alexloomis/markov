{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{- |
Module      : Markov
Description : Realization of Markov processes with known parameters.
Maintainer  : atloomis@math.arizona.edu
Stability   : Experimental

Three type classes for deterministically analyzing
Markov chains with known parameters.
'Markov0' is intended to list possible outcomes,
'Markov' should allow for more sophisticated analysis.
A more general definition can be found in "Markov.Generic"
that allows for containers other than lists.
See "Markov.Example" for examples.
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
     ) where

import Control.Comonad (Comonad, extract)

import qualified Data.List          as DL
import qualified Data.List.NonEmpty as NE

---------------------------------------------------------------
-- Markov0
---------------------------------------------------------------

-- |A basic implementation of Markov chains.
class (Eq s) => Markov0 s where
    transition0 :: s -> [s -> s]
    step0       :: s -> [s]
    transition0 x = const <$> step0 x
    step0 x = ($ x) <$> transition0 x
    {-# MINIMAL transition0 | step0 #-}

-- |Iterated steps, with equal states combined.
chain0 :: Markov0 s => [s] -> [[s]]
chain0 = DL.iterate' $ DL.nub . concatMap step0

---------------------------------------------------------------------------------------
-- Markov
---------------------------------------------------------------------------------------

-- |An implementation of Markov chains.
class (Applicative t, Comonad t) => Markov t s where
    transition :: s -> [t (s -> s)]
    step       :: t s -> [t s]
    sequential :: [s -> [t (s -> s)]]
    transition = fmap (fmap const) . step . pure
    step x = foldr (concatMap . step') [x] sequential
      where step' f y = (<*> y) <$> f (extract y)
    sequential = [transition]
    {-# MINIMAL transition | step | sequential #-}

-- |Iterated steps, with equal states combined using 'summarize' operation.
chain :: (Combine (t s), Ord (t s), Markov t s) => [t s] -> [[t s]]
chain = DL.iterate'
    $ fmap summarize . NE.group . DL.sort . concatMap step

---------------------------------------------------------------------------------------
-- Combine
---------------------------------------------------------------------------------------

-- |Within equivalence classes, @combine@ should be associative,
-- commutative, and idempotent (up to equivalence).
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
-- Merge
---------------------------------------------------------------------------------------

-- Does not group to combine unless equal.
-- |Values from a 'Monoid' which have the respective
-- binary operation applied each step,
-- where different values mean states should not be combined.
-- E.g., strings with concatenation.
newtype Merge a = Merge a
    deriving newtype (Eq, Semigroup, Monoid, Enum, Num, Ord, Fractional, Show)

instance Combine (Merge a) where combine = const

---------------------------------------------------------------------------------------
-- Sum
---------------------------------------------------------------------------------------

-- |Values which are added each step,
-- where different values mean states should not be combined.
-- E.g., number of times a red ball is picked from an urn.
newtype Sum a = Sum a
    deriving newtype (Eq, Enum, Num, Ord, Fractional, Show)

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
    deriving newtype (Enum, Fractional, Integral, Num, Real, Show)

-- Don't sort by probability.
-- |WARNING! Defined @compare _ _ = EQ@!
instance Ord (Product a) where
    compare _ _ = EQ

-- This causes Data.List.group to act more like Data.Discrimination.group
-- |WARNING! Defined @_ == _ = True@!
instance Eq (Product a) where _ == _ = True

instance Num a => Combine (Product a) where combine = (+)

instance Num a => Semigroup (Product a) where x <> y = x * y

instance Num a => Monoid (Product a) where mempty = 1

