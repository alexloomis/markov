{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, DeriveGeneric, DeriveAnyClass,
DerivingStrategies, GeneralizedNewtypeDeriving, FlexibleInstances, TypeOperators #-}
{-|
Module      : Examples
Description : Examples of Markov chains implemented using "Markov".
Maintainer  : atloomis@math.arizona.edu
Stability   : experimental

Several examples of Markov chains, implemented with 'Markov'.
-}
module Examples ( Simple (..)
                , Urn (..)
                , Extinction (..)
                , Tidal (..)
                , FillBin
                , initial
                , expectedLoss
                ) where

import Markov
import Generics.Deriving
import Data.Discrimination
import Data.Monoid

---------------------------------------------------------------
-- From a matrix
---------------------------------------------------------------

-- |An example defined from a matrix.
--
-- >>> chain [pure 't' :: MProd Double Char] !! 100
-- [MProd {prod = 0.50609756097561, pstate = 'a'}
-- ,MProd {prod = 0.2926829268292684, pstate = 'l'}
-- ,MProd {prod = 0.20121951219512202, pstate = 't'}]
-- instance Markov (MProd Double) Char where
    -- transition = let mat = [ [0.4, 0.3, 0.3]
                           -- , [0.2, 0.1, 0.7]
                           -- , [0.9, 0.1, 0.0] ]
                     -- chars = ['a','t','l']
                 -- in fromLists mat chars
    
---------------------------------------------------------------
-- Simple random walk
---------------------------------------------------------------

-- |A simple random walk.
-- Possible outcomes of the first three steps:
--
-- >>> take 3 $ chain0 [Simple 0]
-- [[Simple 0]
-- ,[Simple (-1),Simple 1]
-- ,[Simple (-2),Simple 0,Simple 2]]
--
-- Probability of each outcome:
--
-- >>> take 3 $ chain [pure $ Simple 0 :: MProd Double Simple]
-- [[MProd {prod = 1.0, pstate = Simple 0}]
-- ,[MProd {prod = 0.5, pstate = Simple (-1)},MProd {prod = 0.5, pstate = Simple 1}]
-- ,[MProd {prod = 0.25, pstate = Simple (-2)},MProd {prod = 0.5, pstate = Simple 0},MProd {prod = 0.25, pstate = Simple 2}]]
--
-- Number of ways to achieve each outcome:
--
-- >>> take 3 $ chain [pure $ Simple 0 :: MProd Int Simple]
-- [[MProd {prod = 1, pstate = Simple 0}]
-- ,[MProd {prod = 1, pstate = Simple (-1)},MProd {prod = 1, pstate = Simple 1}]
-- ,[MProd {prod = 1, pstate = Simple (-2)},MProd {prod = 2, pstate = Simple 0},MProd {prod = 1, pstate = Simple 2}]]
--
-- Number of times @pred@ was applied,
-- allowing steps in place (@id@)
-- for more interesting output:
--
-- >>> chain [pure $ Simple 0 :: MSum Int Simple] !! 2
-- [ MSum {total = 0, mstate = Simple 0}
-- , MSum {total = 0, mstate = Simple 1}
-- , MSum {total = 0, mstate = Simple 2}
-- , MSum {total = 1, mstate = Simple (-1)}
-- , MSum {total = 1, mstate = Simple 0}
-- , MSum {total = 2, mstate = Simple (-2)} ]

newtype Simple = Simple Int
    deriving Generic
    deriving newtype (Enum, Eq, Ord, Show)
    deriving anyclass Grouping

instance Markov0 Simple where
    transition0 _ = [pred, succ]

instance Markov (Prod Int) Simple where
    transition _ = [ 1 >*< pred
                   , 1 >*< succ ]

instance Markov (Prod Double) Simple where
    transition _ = [ 0.5 >*< pred
                   , 0.5 >*< succ ]

instance Markov (Sum Int) Simple where
    transition _ = [ 1 >*< pred
                   , 0 >*< id
                   , 0 >*< succ ]

---------------------------------------------------------------
-- Urn model
---------------------------------------------------------------

-- |An urn contains balls of two colors.
-- At each step, a ball is chosen uniformly at random from the urn
-- and a ball of the same color is added.
newtype Urn = Urn (Int,Int)
    deriving Generic
    deriving newtype (Eq, Ord, Show)
    deriving anyclass Grouping

instance Markov (Prod Double) Urn where
    transition x = [ probLeft x >*< addLeft
                   , 1 - probLeft x >*< addRight ]

addLeft :: Urn -> Urn
addLeft  (Urn (a,b)) = Urn (a+1,b)

addRight :: Urn -> Urn
addRight (Urn (a,b)) = Urn (a,b+1)

probLeft :: Fractional a => Urn -> a
probLeft (Urn (a,b)) = (fromIntegral a)/(fromIntegral $ a + b)

---------------------------------------------------------------
-- Tutorial
---------------------------------------------------------------

-- |This is the chain from the README.
newtype Extinction = Extinction Int
    deriving Generic
    deriving newtype (Eq, Num, Ord, Show)
    deriving anyclass Grouping

instance Markov (Sum Int :* Prod Double) Extinction where
    transition x = case x of
        Extinction 0 -> [ 0 >*< (q+r) >*< id
                   , 0 >*< s >*< (+1) ]
        _       -> [ 1 >*< q >*< (\_ -> 0)
                   , 0 >*< r >*< id
                   , 0 >*< s >*< (+1) ]
        where q = 0.1; r = 0.3; s = 0.6

---------------------------------------------------------------
-- More complex random walk
---------------------------------------------------------------

-- |A time inhomogenous random walk that vaguely models tides
-- by periodically switching directions
-- and falling back from a shore at the origin.
data Tidal = Tidal { time     :: Double
                   , position :: Int }
                   deriving (Eq, Ord, Show, Generic)
                   deriving anyclass Grouping

instance Markov (Prod Double) Tidal where
    transition tw = [ Prod (Product (probRight tw)) >*< stepPos (+1)
                    , Prod (Product (1 - (probRight tw))) >*< stepPos (flip (-) 1) ]

stepPos :: (Int -> Int) -> Tidal -> Tidal
stepPos f tw = Tidal (time tw + 1) (f $ position tw)

probRight :: Tidal -> Double
probRight tw = timeBias * positionBias
    where timeBias = (1 + sin (2 * pi * (time tw) / stepsPerCycle))/2
          positionBias
              | position tw >= 0 = 1 / steepness
              | otherwise       = 1
          stepsPerCycle = 10
          steepness     = 1.3 -- Double from 1 (flat) to +infty

---------------------------------------------------------------
-- Yet more complex example
---------------------------------------------------------------

-- |Represents bins with free slots and items.
type Bin   = (Open,Full)
type Index = Int
-- |Represents space between bins where they can expand.
type Gap   = Int
type Full  = Int
type Open  = Int
type Trans = FillBin -> FillBin

-- |A collection of bins with gaps between them.
-- At each step an empty space is chosen
-- form a bin or from a gap.
-- If it is in a bin, the space is filled.
-- If it is in a gap, it is assigned to an adjacent bin,
-- which expands to contain it and any intervening spaces,
-- and then the space filled.
data FillBin = End Gap | Ext Gap Bin FillBin deriving (Eq, Ord, Generic, Grouping)

instance Show FillBin where
    show (Ext g b s) = show g ++ " " ++ show b ++ " " ++ show s
    show (End g) = show g

instance Combine FillBin

instance Markov (Prod Double) FillBin where
    transition x = case probId x of
        0 -> filter (\(Prod y,_) -> y /= 0)
            $  [(Prod (probAdd i x), (addItem i)) | i <- indices]
            ++ [(Prod (probGrowL i x), (addItem i . growLeft  j i))
                | i <- indices, j <- [1..gapN (i-1) x]]
            ++ [(Prod (probGrowR i x), (addItem i . growRight j i))
                | i <- indices, j <- [1..gapN i x]]
        1 -> [pure id]
        _ -> error "Pattern not matched in transition"
        where indices = [1..size x]

-- |>>> fBFromLists [1,3,5,10] [(3,5),(9,9),(8,3)]
-- 1 (3,5) 3 (9,9) 5 (8,3) 10
fBFromLists :: [Gap] -> [Bin] -> FillBin
fBFromLists gaps bins = case (gaps,bins) of
    (g:_  , []  ) -> End g
    ([g]  , _   ) -> End g
    (g:gs , b:bs) -> Ext g b $ fBFromLists gs bs
    ([]   , _   ) -> End 0

-- |Create state where all bins start as (0,0).
--
-- >>> initial [5,7,0]
-- 5 (0,0) 7 (0,0) 0
initial :: [Int] -> FillBin
initial gs = fBFromLists gs $ repeat (0,0)

-- |The number of bins.
size :: FillBin -> Int
size x = case x of
    End _ -> 0
    Ext _ _ s -> 1 + size s

-- |The bins of a state.
getBins :: FillBin -> [Bin]
getBins x = case x of
    End _ -> []
    Ext _ b s -> b:getBins s

-- |The open values of a state.
getOpen :: FillBin -> [Open]
getOpen x = map fst $ getBins x

-- |The open value of the Nth bin.
openN :: Index -> FillBin -> Open
openN i x = (getOpen x)!!(i-1)

-- |The full values of a state.
getFull :: FillBin -> [Full]
getFull x = map snd $ getBins x

-- |The full value of the Nth bin.
fullN :: Index -> FillBin -> Full
fullN i x = (getFull x)!!(i-1)

-- |The gap values of a state.
getGap :: FillBin -> [Gap]
getGap x = case x of
    End g -> [g]
    Ext g _ s -> g:getGap s

-- |Warning! Indexed from zero!
gapN :: Index -> FillBin -> Gap
gapN i x = (getGap x)!!i

-- |The command @iApply i f s@ is analagous to
-- @take i s ++ f (drop i s)@.
iApply :: Trans -> Index -> Trans
iApply f idx x = case (idx,x) of
    (1, y) -> f y
    (i, Ext g b s) -> Ext g b $ iApply f (i-1) s
    _ -> error "Pattern not matched in iApply"

-- |Add an item to the ith bin.
addItem :: Index -> Trans
addItem = iApply h
    where h (Ext g (o,f) s) = Ext g (o-1,f+1) s
          h _ = error "pattern not matched in h in addItem"

-- |Expand the ith bin to the left by j.
-- The Markov chain will use @addItem i . growLeft j i@.
growLeft :: Int -> Index -> Trans
growLeft j = iApply h
    where h (Ext g (o,f) s) = Ext (g-j) (o+j,f) s
          h _ = error "pattern not matched in h in growLeft"

growRight :: Int -> Index -> Trans
growRight j = iApply h
    where h (Ext g (o,f) s) = Ext g (o+j,f) (shrink s)
          h _ = error "pattern not matched in h in growRight"
          shrink s = case s of
              End g -> End (g-j)
              Ext g b t -> Ext (g-j) b t

-- |The sum of all open slots in bins and gaps.
slots :: FillBin -> Int
slots x = sum $ getGap x ++ getOpen x

-- |The probability that a state returns to itself.
probId :: Num a => FillBin -> a
probId x = case slots x == 0 of
    True  -> 1
    False -> 0

divInt :: (Integral a, Integral b, Fractional c) => a -> b -> c
divInt x y = (fromIntegral x)/(fromIntegral y)

-- |The probability that the ith bin gains an item.
probAdd :: Fractional a => Index -> FillBin -> a
probAdd i x = openN i x `divInt` slots x

-- |The probability that the ith bin expands to the left.
probGrowL :: Fractional a => Index -> FillBin -> a
probGrowL i x = case test of
    True  -> 1 `divInt` slots x
    False -> 0
    where test = i == 1 || fullN i x < fullN (i-1) x

-- |The probability that the ith bin expands to the right.
probGrowR :: Fractional a => Index -> FillBin -> a
probGrowR i x = case test of
    True  -> 1 `divInt` slots x
    False -> 0
    where test = i == size x || fullN i x <= fullN (i+1) x

---------------------------------------------------------------
-- Several functions to help study the previous process
---------------------------------------------------------------

-- |The \(l^2\) distance between a finished state
-- and a state with perfectly balanced bins.
individualLoss :: Fractional a => FillBin -> a
individualLoss x = sum . map f . getFull $ x
    where f y = (fromIntegral y - ideal)^2
          ideal = sum (getFull x) `divInt` size x

probLoss :: Fractional a => (Prod a, FillBin) -> a
probLoss (Prod x, y) = getProduct x * individualLoss y

-- |Expected loss of a set of pstates of @['FillBin']@.
-- Loss is the \(l^2\) distance between a finished state
-- and a state with perfectly balanced bins.
--
-- >>> expectedLoss [MProd (1.0 :: Double) $ initial [1,0,3]]
-- 2.0
expectedLoss :: (Fractional a, Markov (Prod a) FillBin) => [(Prod a, FillBin)] -> a
expectedLoss xs = sum . map probLoss $ (chain xs) !! idx
    where idx = slots . snd . head $ xs
