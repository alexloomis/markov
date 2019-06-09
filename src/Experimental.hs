{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies, FlexibleInstances,
GeneralizedNewtypeDeriving #-}
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

---------------------------------------------------------------
-- Markov
---------------------------------------------------------------

-- |An implementation of markov chains.
-- Instances of Markov should follow the laws:
--
-- prop> x <> x == x
class (Track t, Ord m) => Markov t m where
    transition :: m -> [Status t (m -> m)]
    step       :: Status t m -> [Status t m]
    chain      :: [Status t m] -> [[Status t m]]
    step x = fmap (x <**>) (transition $ status x)
    chain  = DL.iterate' $ map sconcat . NE.group . DL.sort . concatMap step

---------------------------------------------------------------
-- DProd
---------------------------------------------------------------

class DProd a b where
    data Prod a b :: *
    projl :: Prod a b -> a
    projr :: Prod a b -> b
    dprod :: a -> b -> Prod a b

instance (DProd a b, Show a, Show b) => Show (Prod a b) where
    show x = "(" ++ show (projl x) ++ ", " ++ show (projr x) ++ ")"
instance (DProd a b, Eq a, Eq b) => Eq (Prod a b) where
    x == y = projl x == projl y && projr x == projr y
instance (DProd a b, Ord a, Ord b) => Ord (Prod a b) where
    compare x y = compare (projl x) (projl y) <> compare (projr x) (projr y)
instance (DProd a b, Semigroup a, Semigroup b) => Semigroup (Prod a b) where
    x <> y = dprod (projl x <> projl y) (projr x <> projr y)
instance (DProd a b, Monoid a, Monoid b) => Monoid (Prod a b) where
    mempty = dprod mempty mempty
instance (DProd a b, Track a, Track b) => Track (Prod a b) where
    data Status (Prod a b) c = Status0 {alpha0 :: Prod a b, beta0 :: c}
    combine x y = dprod (combine (projl x) (projl y)) (combine (projr x) (projr y))
    track = alpha0
    status = beta0
    build = Status0

---------------------------------------------------------------
-- Track
---------------------------------------------------------------

-- @combine@ should be commutative and associative, and:
-- |prop> combine x x == x
-- |prop> build (track x) (status x) = x
class (Ord track, Monoid track) => Track track where
    data Status track :: * -> *
    combine :: track -> track -> track
    track   :: Status track status -> track
    status  :: Status track status -> status
    build   :: track -> status -> Status track status

instance (Track a, Show a, Show b) => Show (Status a b) where
    show x = show (status x) ++ ": " ++ show (track x)
instance (Eq b, Track a) => Eq (Status a b) where
    x == y = track x == track y && status x == status y
instance (Ord b, Track a) => Ord (Status a b) where
    compare x y = compare (status x) (status y) <> compare (track x) (track y)
instance Track a => Functor (Status a) where
    fmap f x = build (track x) (f $ status x)
instance Track a => Applicative (Status a) where
    pure x = build mempty x
    f <*> x = build (track f <> track x) (status f $ status x)
instance Track a => Semigroup (Status a b) where
    x <> y = build (combine (track x) (track y)) (status x)

---------------------------------------------------------------
-- Track -- MCon
---------------------------------------------------------------

data MCon a = MCon a deriving Eq
instance Show a => Show (MCon a) where
    show (MCon a) = show a
instance Ord a => Ord (MCon a) where
    compare (MCon a) (MCon b) = compare a b
instance Semigroup (MCon String) where
    MCon x <> MCon y = MCon (x <> y)
instance Monoid (MCon String) where
    mempty = MCon mempty

instance Track (MCon String) where
    data Status (MCon String) a = Status1 {alpha1 :: MCon String, beta1 :: a}
    combine x _ = x
    track = alpha1
    status = beta1
    build = Status1

---------------------------------------------------------------
-- Track -- MProd
---------------------------------------------------------------

data MSum a = MSum a deriving (Eq, Ord)
instance Show a => Show (MSum a) where
    show (MSum a) = show a
instance Num a => Semigroup (MSum a) where
    MSum x <> MSum y = MSum (x + y)
instance Num a => Monoid (MSum a) where
    mempty = MSum 0

instance (Num a, Ord a) => Track (MSum a) where
    data Status (MSum a) b = Status4 {alpha4 :: MSum a, beta4 :: b}
    combine x _ = x
    track = alpha4
    status = beta4
    build = Status4

---------------------------------------------------------------
-- Track -- MProd
---------------------------------------------------------------

data MProd a = MProd a
instance Eq (MProd a) where
    _ == _ = True
instance Ord (MProd a) where
    compare _ _ = EQ
instance Show a => Show (MProd a) where
    show (MProd a) = show a
instance Num a => Semigroup (MProd a) where
    MProd x <> MProd y = MProd (x * y)
instance Num a => Monoid (MProd a) where
    mempty = MProd 1

instance Num a => Track (MProd a) where
    data Status (MProd a) b = Status2 {alpha2 :: MProd a, beta2 :: b}
    combine (MProd x) (MProd y) = MProd (x + y)
    track = alpha2
    status = beta2
    build = Status2

---------------------------------------------------------------
-- Track -- DProd MCon MProd
---------------------------------------------------------------

instance DProd (MCon String) (MProd Double) where
    data Prod (MCon String) (MProd Double) = Prod3 {alpha3 :: MCon String, beta3 :: MProd Double}
    projl = alpha3
    projr = beta3
    dprod = Prod3

newtype TestWalk = TestWalk Int deriving (Enum, Eq, Ord, Show)
instance Markov (Prod (MCon String) (MProd Double)) TestWalk where
    transition _ = [ build (dprod (MCon "l") (MProd 0.5)) pred
                   , build (dprod (MCon "r") (MProd 0.5)) succ ]
short1 = dprod (MCon "") (MProd 1 :: MProd Double)
short2 = build short1 (TestWalk 5)

instance DProd (MSum Int) (MProd Double) where
    data Prod (MSum Int) (MProd Double) = Prod5 {alpha5 :: MSum Int, beta5 :: MProd Double}
    projl = alpha5
    projr = beta5
    dprod = Prod5

instance Markov (Prod (MSum Int) (MProd Double)) TestWalk where
    transition _ = [ build (dprod (MSum 1) (MProd 0.5)) pred
                   , build (dprod (MSum 0) (MProd 0.5)) succ ]
short3 = dprod (MSum 0 :: MSum Int) (MProd 1 :: MProd Double)
short4 = build short3 (TestWalk 5)
