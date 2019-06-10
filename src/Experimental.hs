{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies, FlexibleInstances,
GeneralizedNewtypeDeriving, DeriveGeneric, DeriveAnyClass #-}
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
import Generics.Deriving as GD
import Data.Discrimination as DD

---------------------------------------------------------------
-- Markov
---------------------------------------------------------------

class (Grouping (Status t m), Track t) => MarkovGeneric t m where
    gentransition :: m -> [Status t (m -> m)]
    genstep       :: Status t m -> [Status t m]
    genchain      :: [Status t m] -> [[Status t m]]
    genstep x = fmap (x <**>) (gentransition $ status x)
    genchain  = DL.iterate' $ map (sconcat . NE.fromList) . DD.group . concatMap genstep
    -- WRONG since DD.group ignores equivalence class
    -- Use groupWith and map to void for trivial groupings?

---------------------------------------------------------------
-- DProd
---------------------------------------------------------------

data Prod a b = Prod {projl :: a, projr :: b} deriving (Generic, Grouping)
dprod :: a -> b -> Prod a b
dprod = Prod

instance (Show a, Show b) => Show (Prod a b) where
    show x = "(" ++ show (projl x) ++ ", " ++ show (projr x) ++ ")"
instance (Eq a, Eq b) => Eq (Prod a b) where
    x == y = projl x == projl y && projr x == projr y
instance (Ord a, Ord b) => Ord (Prod a b) where
    compare x y = compare (projl x) (projl y) <> compare (projr x) (projr y)
instance (Semigroup a, Semigroup b) => Semigroup (Prod a b) where
    x <> y = dprod (projl x <> projl y) (projr x <> projr y)
instance (Monoid a, Monoid b) => Monoid (Prod a b) where
    mempty = dprod mempty mempty
instance (Track a, Track b) => Track (Prod a b) where
    data Status (Prod a b) c = Status0 { alpha0 :: Prod a b
                                       , beta0 :: c }
                                       deriving (Generic, Grouping)
    combine x y = dprod (combine (projl x) (projl y)) (combine (projr x) (projr y))
    track = alpha0
    status = beta0
    build = Status0

---------------------------------------------------------------
-- Track
---------------------------------------------------------------

-- @combine@ should be associative, commutative, and,
-- up to equivalence, idempotent. Given
--
-- > data Status (Type a) b = T { alpha :: Type a
-- >                            , beta :: b }
--
-- we should have
--
-- > build = T
-- > track = alpha
-- > status = beta
--
-- I.e. the following laws hould hold:
--
-- |prop> (x `combine y) `combine` z = x `combine` (y `combine z)
-- |prop> combine x y = combine y x
-- |prop> combine x x == x
-- |prop> x = build (track x) (status x)
class (Eq track, Monoid track) => Track track where
    data Status track :: * -> *
    combine :: track -> track -> track
    track   :: Status track status -> track
    status  :: Status track status -> status
    build   :: track -> status -> Status track status

instance (Track a, Show a, Show b) => Show (Status a b) where
    show x = show (status x) ++ ": " ++ show (track x)
instance (Eq b, Track a) => Eq (Status a b) where
    x == y = track x == track y && status x == status y
instance Track a => Functor (Status a) where
    fmap f x = build (track x) (f $ status x)
instance Track a => Applicative (Status a) where
    pure x = build mempty x
    f <*> x = build (track f <> track x) (status f $ status x)
instance Track a => Semigroup (Status a b) where
    x <> y = build (combine (track x) (track y)) (status x)

---------------------------------------------------------------
-- NoCombine
---------------------------------------------------------------

data NoCombine a = NoCombine a deriving (Eq, Generic, Grouping)
instance Show a => Show (NoCombine a) where
    show (NoCombine a) = show a
instance Semigroup a => Semigroup (NoCombine a) where
    NoCombine x <> NoCombine y = NoCombine (x <> y)
instance Monoid a => Monoid (NoCombine a) where
    mempty = NoCombine mempty

instance (Eq a, Monoid a) => Track (NoCombine a) where
    data Status (NoCombine a) b = Status1 { alpha1 :: NoCombine a
                                          , beta1 :: b }
                                          deriving (Generic, Grouping)
    combine x _ = x
    track = alpha1
    status = beta1
    build = Status1

---------------------------------------------------------------
-- Track -- Combine
---------------------------------------------------------------

data Combine a = Combine a deriving (Generic, Grouping)
instance Show a => Show (Combine a) where
    show (Combine a) = show a
instance Eq (Combine a) where
    _ == _ = True
instance Semigroup a => Semigroup (Combine a) where
    Combine x <> Combine y = Combine (x <> y)
instance Monoid a => Monoid (Combine a) where
    mempty = Combine mempty

instance Monoid a => Track (Combine a) where
    data Status (Combine a) b = Status7 { alpha7 :: Combine a
                                        , beta7 :: b }
                                        deriving (Generic, Grouping)
    combine x y = x <> y
    track = alpha7
    status = beta7
    build = Status7


---------------------------------------------------------------
-- Track -- MSum
---------------------------------------------------------------

data MSum a = MSum a deriving (Eq, Generic, Grouping)
instance Show a => Show (MSum a) where
    show (MSum a) = show a
instance Num a => Semigroup (MSum a) where
    MSum x <> MSum y = MSum (x + y)
instance Num a => Monoid (MSum a) where
    mempty = MSum 0

instance (Eq a, Num a) => Track (MSum a) where
    data Status (MSum a) b = Status4 { alpha4 :: MSum a
                                     , beta4 :: b }
                                     deriving (Generic, Grouping)
    combine x _ = x
    track = alpha4
    status = beta4
    build = Status4


---------------------------------------------------------------
-- Track -- MProd
---------------------------------------------------------------

data MProd a = MProd a deriving (Generic, Grouping)
instance Eq (MProd a) where
    _ == _ = True
instance Show a => Show (MProd a) where
    show (MProd a) = show a
instance Num a => Semigroup (MProd a) where
    MProd x <> MProd y = MProd (x * y)
instance Num a => Monoid (MProd a) where
    mempty = MProd 1

instance Num a => Track (MProd a) where
    data Status (MProd a) b = Status2 { alpha2 :: MProd a
                                      , beta2 :: b }
                                      deriving (Generic, Grouping)
    combine (MProd x) (MProd y) = MProd (x + y)
    track = alpha2
    status = beta2
    build = Status2

---------------------------------------------------------------
-- Test
---------------------------------------------------------------

newtype TestWalk = TestWalk Int deriving (Enum, Eq, Generic, Grouping, Ord, Show)

instance MarkovGeneric (MSum Int) TestWalk where
    gentransition _ = [ build (MSum (-1)) pred
                      , build (MSum 0) succ ]
short1 = MSum 0 :: MSum Int
short2 = build short1 (TestWalk 0)

instance MarkovGeneric (Prod (NoCombine String) (MProd Int)) TestWalk where
    gentransition _ = [ build (dprod (NoCombine "l") (MProd 1)) pred
                      , build (dprod (NoCombine "r") (MProd 1)) succ ]
short3 = dprod (NoCombine "") (MProd 1 :: MProd Int)
short4 = build short3 (TestWalk 5)

instance MarkovGeneric (MProd Int) TestWalk where
    gentransition _ = [ build (MProd 1) pred
                      , build (MProd 1) succ ]
short5 = MProd 1 :: MProd Int
short6 = build short5 (TestWalk 0)
{-
---------------------------------------------------------------
-- Track -- DProd NoCombine MProd
---------------------------------------------------------------

-- instance DProd (NoCombine String) (MProd Double) where
    -- data Prod (NoCombine String) (MProd Double) = Prod3 {alpha3 :: NoCombine String, beta3 :: MProd Double}
    -- projl = alpha3
    -- projr = beta3
    -- dprod = Prod3

newtype TestWalk = TestWalk Int deriving (Enum, Eq, Ord, Show)
-- instance Markov (Prod (NoCombine String) (MProd Double)) TestWalk where
    -- transition _ = [ build (dprod (NoCombine "l") (MProd 0.5)) pred
                   -- , build (dprod (NoCombine "r") (MProd 0.5)) succ ]
-- short1 = dprod (NoCombine "") (MProd 1 :: MProd Double)
-- short2 = build short1 (TestWalk 5)

-- instance DProd (MSum Int) (MProd Double) where
    -- data Prod (MSum Int) (MProd Double) = Prod5 {alpha5 :: MSum Int, beta5 :: MProd Double}
    -- projl = alpha5
    -- projr = beta5
    -- dprod = Prod5

instance Markov (Prod (MSum Int) (MProd Double)) TestWalk where
    transition _ = [ build (dprod (MSum 1) (MProd 0.5)) pred
                   , build (dprod (MSum 0) (MProd 0.5)) succ ]
short3 = dprod (MSum 0 :: MSum Int) (MProd 1 :: MProd Double)
short4 = build short3 (TestWalk 5)
-}
