{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
{-|
Module      : Markov
Description : Deterministic analysis of Markov processes.
Maintainer  : atloomis@math.arizona.edu
Stability   : experimental

Two type classes for deterministically analyzing Markov chains.
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
              -- *MProd
              , MProd (..)
              , randomMProd
              , randomPath
              , fromLists
              -- *MSum
              , MSum (..)
              -- *MNull
              , MNull (..)
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
    step0       :: m -> [m]
    -- |Iterated steps.
    chain0      :: [m] -> [[m]]
    step0 x = fmap ($ x) (transition0 x)
    chain0  = DL.iterate' $ DL.nub . concatMap step0

---------------------------------------------------------------
-- Markov
---------------------------------------------------------------

-- |An implementation of markov chains.
-- Instances of Markov should follow the law:
--
-- prop> transition (f x) == transition (pure x)
class Applicative f => Markov f m where
    -- |The transition functions from a state, and the probability of each transition.
    transition :: f m -> [f (m -> m)]
    step       :: f m -> [f m]
    -- |Iterated steps with equal states combined using 'Semigroup' operation.
    -- Requires 'Ord' instead of 'Eq' to greatly speed up implementation,
    -- order can be arbitrary though.
    chain      :: (Ord (f m), Semigroup (f m)) => [f m] -> [[f m]]
    step x = fmap (x <**>) (transition x)
    chain  = DL.iterate' $ map sconcat . gather . concatMap step
        where gather = NE.group . DL.sort

---------------------------------------------------------------
-- MProd
---------------------------------------------------------------

-- |An applicative to be used with Markov for basic stochastic analysis.
-- Tracks a multiplicative value.
data MProd a b = MProd { prod   :: a
                       , pstate :: b }
                       deriving Show

-- |MProds are equal if they contain the same 'pstate', ignoring 'prod'.
instance (Eq b) => Eq (MProd a b) where
    (==) x y = pstate x == pstate y

instance (Ord b) => Ord (MProd a b) where
    compare x y = compare (pstate x) (pstate y)

instance Functor (MProd a) where
    fmap f x = MProd { prod = prod x
                     , pstate = f $ pstate x }

instance Num a => Applicative (MProd a) where
    pure x = MProd { prod = 1
                   , pstate = x }
    (<*>) f x = MProd { prod = prod f * prod x
                      , pstate = pstate f $ pstate x }

instance Num a => Monad (MProd a) where
    (>>=) x f = MProd { prod = prod x * (prod . f . pstate  $ x)
                      , pstate = pstate . f . pstate $ x }

-- \(P[X_{k+1} = x_0] = \sum_{x_i} P[X_{k+1} = x_0 \mid X_k = x_i]\).
instance Num a => Semigroup (MProd a b) where
    (<>) x y = MProd { prod = prod x + prod y
                     , pstate = pstate x }

-- |Randomly chooses an 'MProd' by probability.
randomMProd :: (Real a, MR.MonadRandom m) => [MProd a b] -> m (MProd a b)
randomMProd xs = MR.fromList . map (\x -> (x, toRational $ prod x)) $ xs

-- |Returns a single realization of a Markov chain.
randomPath :: (Markov (MProd a) b, Real a, MR.RandomGen g) => MProd a b -> g -> [MProd a b]
randomPath x g = map (flip MR.evalRand g) . iterate (>>= (randomMProd . step)) $ pure x

-- |Create a transition function from a transition matrix.
-- If [[a]] is an n x n matrix, length [b] should be n.
fromLists :: Eq  b => [[a]] -> [b] -> MProd a b -> [MProd a (b -> b)]
fromLists matrix states (MProd _ b) = case DL.elemIndex b states of
    Nothing -> []
    Just n  -> zipWith MProd (matrix!!n) toState
    where toState = map (\x -> (\_ -> x)) states

---------------------------------------------------------------
-- MSum
---------------------------------------------------------------

-- |An applicative to be used with Markov.
-- Tracks an additive value.
data MSum a b = MSum { total :: a
                     , mstate :: b }
                     deriving (Eq, Ord, Show)

instance Functor (MSum a) where
    fmap f x = MSum { total = total x
                    , mstate = f $ mstate x }

instance Num a => Applicative (MSum a) where
    pure x = MSum { total = 0
                  , mstate = x }
    (<*>) f x = MSum { total = total f + total x
                     , mstate = mstate f $ mstate x }

instance Num a => Monad (MSum a) where
    (>>=) x f = MSum { total = total x + (total . f . mstate $ x)
                     , mstate = mstate . f . mstate $ x }

instance Num a => Semigroup (MSum a b) where
    (<>) x _ = x

---------------------------------------------------------------
-- MNull
---------------------------------------------------------------

-- |An applicative for Markov that does not track anything.
-- @instance Markov MNull a@
-- is the same as @instance Markov0 a@
-- Since 'Markov0' is faster, it should usually be prefered.
newtype MNull a = MNull {estate :: a} deriving (Eq, Ord, Show)

instance Functor MNull where
    fmap f (MNull a) = MNull $ f a

instance Applicative MNull where
    pure x = MNull x
    (<*>) (MNull f) x = fmap f x

instance Eq a => Semigroup (MNull a) where
    (<>) x _ = x
