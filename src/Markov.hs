{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveGeneric,
DeriveAnyClass, DerivingStrategies, TypeOperators #-}
{-|
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
              -- *Markov
              , Markov (..)
              -- *MultiMarkov
              , randomProduct
              , randomPath
              , MultiMarkov (..)
              -- *Combine
              , Combine (..)
              , Merge (..)
              , Sum (..)
              , Product (..)
              -- *Misc
              , (:*)
              , (>*<)
              , fromLists
              -- *Testing
              ) where

import Markov.Instances ()
import Control.Applicative ((<**>))
import Generics.Deriving (Generic)
import Data.Discrimination (Grouping, grouping)
import qualified Data.Discrimination as DD
import qualified Data.List as DL
import qualified Data.List.NonEmpty as NE
import qualified Data.Functor.Contravariant as FC
import qualified Control.Monad.Random as MR

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
-- To speed up @chain@, try instead:
--
-- > chain = DL.iterate' $ map summarize' . NE.group . DL.sort . concatMap step
-- >     where summarize' xs@((_,b)NE.:|_) = (summarize . fmap fst $ xs, b)
class (Combine t, Grouping t, Grouping m, Monoid t) => Markov t m where
    transition :: m -> [(t, m -> m)]
    step       :: (t,m) -> [(t,m)]
    chain      :: [(t,m)] -> [[(t,m)]]
    step x = fmap (x <**>) (transition $ snd x)
    -- |Iterated steps, with equal states combined using 'summarize' operation.
    chain  = DL.iterate' $ map (summarize' . NE.fromList)
             . DD.group . concatMap step
             where summarize' xs@((_,b)NE.:|_) = (summarize . fmap fst $ xs, b)
             -- WARNING: DD.group does not currently respect equivalence classes.

---------------------------------------------------------------------------------------
-- Multi-Transition Markov
---------------------------------------------------------------------------------------

-- |An implementation of Markov chains that allows multi-transition steps.
class (Combine m, Grouping m, Semigroup m) => MultiMarkov m where
    multiTransition :: m -> [m -> [m]]
    multiStep       :: m -> [m]
    multiChain      :: [m] -> [[m]]
    multiStep x = foldr phi [x] (multiTransition x)
        where phi f = concatMap (delta f)
              delta f y = map (y <>) (f y)
    multiChain  = DL.iterate' $ map (summarize . NE.fromList)
                  . DD.group . concatMap multiStep

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
-- binary operation applied each step.
-- E.g., strings with concatenation.
newtype Merge a = Merge a
    deriving (Eq, Generic)
    deriving newtype (Semigroup, Monoid, Enum, Num, Fractional, Show)
    deriving anyclass Grouping

instance Combine (Merge a) where
    combine = const

---------------------------------------------------------------------------------------
-- Sum
---------------------------------------------------------------------------------------

-- |Values which are added each step.
-- E.g., number of times a red ball is picked from an urn.
newtype Sum a = Sum a
    deriving Generic
    deriving newtype (Eq, Enum, Num, Fractional, Show)
    deriving anyclass Grouping

instance Combine (Sum a) where
    combine = const

instance Num a => Semigroup (Sum a) where
    x <> y = x + y

instance Num a => Monoid (Sum a) where
    mempty = 0

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
instance Eq (Product a) where
    _ == _ = True

instance Num a => Combine (Product a) where
    combine = (+)

instance Num a => Semigroup (Product a) where
    x <> y = x * y

instance Num a => Monoid (Product a) where
    mempty = 1

---------------------------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------------------------

-- |Randomly choose from a list by probability.
randomProduct :: (Real a, MR.MonadRandom m) => [(a, b)] -> m (a, b)
randomProduct xs = MR.fromList . map (\x -> (x, toRational $ fst x)) $ xs

-- |Returns a single realization of a Markov chain.
randomPath :: (Markov a b, Real a, MR.RandomGen g) => (a,b) -> g -> [(a,b)]
randomPath x g = map (flip MR.evalRand g) . iterate (>>= (randomProduct . step)) $ pure x

-- |Create a transition function from a transition matrix.
-- If [[a]] is an n x n matrix, length [b] should be n.
fromLists :: Eq  b => [[a]] -> [b] -> b -> [(a, b -> b)]
fromLists matrix states b = case DL.elemIndex b states of
    Nothing -> []
    Just n  -> zip (matrix!!n) toState
    where toState = map const states
