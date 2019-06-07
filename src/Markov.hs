{-# LANGUAGE MultiParamTypeClasses, ConstrainedClassMethods, FlexibleInstances,
FlexibleContexts #-}
{-|
Module      : Markov
Description : A class for Markov processes.
Maintainer  : atloomis@math.arizona.edu
Stability   : experimental

Two type classes for implementing Markov chains.
'Markov0' is intended to list possible outcomes.
'Markov' should allow for more sophisticated analysis.
See "Examples" for examples.
-}
module Markov (
              -- *Markov0
              Markov0 (..)
              -- *Markov
              , Markov (..)
              -- *Event
              , Event (..)
              , randomEvent
              , randomPath
              -- *Count
              , Count (..)
              ) where

import Control.Applicative
import Data.Semigroup
import qualified Data.List as DL
import qualified Data.List.NonEmpty as NE
import qualified Control.Monad.Random as MR

---------------------------------------------------------------
-- Markov0
---------------------------------------------------------------

-- |A basic implementation of Markov chains.
class (Eq m) => Markov0 m where
    -- |The transition functions from a state, and the probability of each transition.
    transition0 :: m -> [m -> m]
    -- The possible outcomes from an 'Event'.
    step0       :: m -> [m]
    -- |Iterated steps.
    chain0      :: [m] -> [[m]]
    step0 x = fmap ($ x) (transition0 x)
    chain0  = DL.iterate' $ DL.nub . concatMap step0

---------------------------------------------------------------
-- Markov
---------------------------------------------------------------

-- |An implementation of markov chains.
class Applicative f => Markov f m where
    -- |The transition functions from a state, and the probability of each transition.
    transition :: f m -> [f (m -> m)]
    step       :: f m -> [f m]
    -- |Iterated steps, without duplicate states combined.
    links      :: [f m] -> [[f m]]
    -- |Iterated steps with equal states combined using 'Semigroup' operation.
    -- Requires 'Ord' instead of 'Eq' to greatly speed up implementation,
    -- order can be arbitrary though.
    chain      :: (Ord (f m), Semigroup (f m)) => [f m] -> [[f m]]
    step x = fmap (x <**>) (transition x)
    links  = iterate $ concatMap step
    chain  = DL.iterate' $ map sconcat . gather . concatMap step
        where gather = NE.group . DL.sort

---------------------------------------------------------------
-- Event
---------------------------------------------------------------

-- |An applicative to be used with Markov for basic stochastic analysis.
data Event a b = Event { prob  :: a -- ^This should usually be a number between 0 and 1, inclusive.
                       , event :: b }
                       deriving Show

-- |Events are equal if they contain the same 'event', ignoring 'prob'.
instance (Eq b) => Eq (Event a b) where
    (==) x y = event x == event y

instance (Ord b) => Ord (Event a b) where
    compare x y = compare (event x) (event y)

instance Functor (Event a) where
    fmap f x = Event { prob  = prob x
                     , event = f $ event x }

-- Given \(x\) a probability of a transition
-- and \(y\) a probability of a event,
-- the probability of being in that event
-- and performing the transition is \(xy\).
instance Num a => Applicative (Event a) where
    pure x = Event { prob  = 1
                   , event = x }
    (<*>) f x = Event { prob  = prob f * prob x
                      , event = event f $ event x }

instance Num a => Monad (Event a) where
    (>>=) x f = Event { prob  = prob x * (prob . f . event $ x)
                      , event = event . f . event $ x }

-- Let \(X_k\) be the event of a chain at time \(k\).
-- Then \(P[X_{k+1} = x_0] = \sum_{x_i} P[X_{k+1} = x_0 \mid X_k = x_i]\).
instance Num a => Semigroup (Event a b) where
    (<>) x y = Event { prob  = prob x + prob y
                     , event = event x }

-- |Randomly chooses an 'Event' by probability.
randomEvent :: (Real a, MR.MonadRandom m) => [Event a b] -> m (Event a b)
randomEvent xs = MR.fromList . map (\x -> (x, toRational $ prob x)) $ xs

-- |Returns a single realization of a Markov chain.
randomPath :: (Markov (Event a) b, Real a, MR.RandomGen g) => Event a b -> g -> [Event a b]
randomPath x g = map (flip MR.evalRand g) . iterate (>>= (randomEvent . step)) $ pure x

---------------------------------------------------------------
-- Count
---------------------------------------------------------------

-- |An example applicative to be used with Markov.
data Count a b = Count { total :: a
                       , state :: b }
                       deriving (Eq, Ord, Show)
instance Functor (Count a) where
    fmap f x = Count { total = total x
                     , state = f $ state x }
instance Num a => Applicative (Count a) where
    pure x = Count { total = 0
                   , state = x }
    (<*>) f x = Count { total = total f + total x
                      , state = state f $ state x }
instance Num a => Monad (Count a) where
    (>>=) x f = Count { total = total x + (total . f . state $ x)
                      , state = state . f . state $ x }
instance Num a => Semigroup (Count a b) where
    (<>) x _ = x
