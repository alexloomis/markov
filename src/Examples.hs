{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts,
GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-|
Module      : Examples
Description : Examples of Markov chains implemented using "Markov".
Maintainer  : atloomis@math.arizona.edu
Stability   : experimental

Several examples of Markov chains,
implemented with 'Markov'.
-}
module Examples ( Simple (..)
                , Urn (..)
                , Tidal (..)
                , State
                , initial
                , expectedLoss
                ) where

import Markov

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
-- >>> take 3 $ chain [pure $ Simple 0 :: Event Double Simple]
-- [[Event {prob = 1.0, event = Simple 0}]
-- ,[Event {prob = 0.5, event = Simple (-1)},Event {prob = 0.5, event = Simple 1}]
-- ,[Event {prob = 0.25, event = Simple (-2)},Event {prob = 0.5, event = Simple 0},Event {prob = 0.25, event = Simple 2}]]
--
-- Number of ways to achieve each outcome:
--
-- >>> take 3 $ chain [pure $ Simple 0 :: Event Int Simple]
-- [[Event {prob = 1, event = Simple 0}]
-- ,[Event {prob = 1, event = Simple (-1)},Event {prob = 1, event = Simple 1}]
-- ,[Event {prob = 1, event = Simple (-2)},Event {prob = 2, event = Simple 0},Event {prob = 1, event = Simple 2}]]
--
-- Number of times @pred@ was applied,
-- allowing steps in place (@id@)
-- for more interesting output:
--
-- >>> chain [pure $ Simple 0 :: Count Int Simple] !! 2
-- [ Count {total = 0, state = Simple 0}
-- , Count {total = 0, state = Simple 1}
-- , Count {total = 0, state = Simple 2}
-- , Count {total = 1, state = Simple (-1)}
-- , Count {total = 1, state = Simple 0}
-- , Count {total = 2, state = Simple (-2)} ]

newtype Simple = Simple Int deriving (Enum, Eq, Ord, Show)
instance Markov0 Simple where
    transition0 _ = [pred, succ]
instance Markov (Event Int) Simple where
    transition _ = [ Event 1 pred
                   , Event 1 succ ]
instance Markov (Event Double) Simple where
    transition _ = [ Event 0.5 pred
                   , Event 0.5 succ ]
-- Ex: @chain [pure $ Simple 0 :: Count Int Simple] !! 3@
-- returns number of @pred@ applied at each possible outcome at time 3.
instance Markov (Count Int) Simple where
    transition _ = [ Count 1 pred
                   , Count 0 id
                   , Count 0 succ ]

---------------------------------------------------------------
-- Urn model
---------------------------------------------------------------

-- |An urn contains balls of two colors.
-- At each step, a ball is chosen uniformly at random from the urn
-- and a ball of the same color is added.
newtype Urn = Urn (Int,Int) deriving (Eq, Ord, Show)
instance Markov (Event Double) Urn where
    transition (Event _ x) = [ Event (probLeft x)     addLeft
                             , Event (1 - probLeft x) addRight]

addLeft  (Urn (a,b)) = Urn (a+1,b)
addRight (Urn (a,b)) = Urn (a,b+1)
probLeft (Urn (a,b)) = (fromIntegral a)/(fromIntegral $ a + b)

---------------------------------------------------------------
-- More complex random walk
---------------------------------------------------------------

-- |A time inhomogenous random walk that vaguely models tides
-- by periodically switching directions
-- and falling back from a shore at the origin.
data Tidal = Tidal { time     :: Double
                   , position :: Int }
                   deriving (Eq, Ord, Show)
instance Markov (Event Double) Tidal where
    transition (Event _ tw) = [ Event (probRight tw) $ stepPos (+1)
                              , Event (1 - (probRight tw)) $ stepPos (flip (-) 1) ]

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
type Trans = State -> State

-- |A collection of bins with gaps between them.
-- At each step an empty space is chosen
-- form a bin or from a gap.
-- If it is in a bin, the space is filled.
-- If it is in a gap, it is assigned to an adjacent bin,
-- which expands to contain it and any intervening spaces,
-- and then the space filled.
data State = End Gap | Ext Gap Bin State deriving (Eq, Ord)
instance Show State where
    show (Ext g b s) = show g ++ " " ++ show b ++ " " ++ show s
    show (End g) = show g
instance Markov (Event Double) State where
    transition (Event _ x) = case probId x of
        0 -> filter (\(Event y _) -> y /= 0)
            $  [Event (probAdd i x) (addItem i) | i <- indices]
            ++ [Event (probGrowL i x) (addItem i . growLeft  j i)
                | i <- indices, j <- [1..gapN (i-1) x]]
            ++ [Event (probGrowR i x) (addItem i . growRight j i)
                | i <- indices, j <- [1..gapN i x]]
        1 -> [pure id]
        _ -> error "Pattern not matched in transition"
        where indices = [1..size x]

-- |>>> fromLists [1,3,5,10] [(3,5),(9,9),(8,3)]
-- 1 (3,5) 3 (9,9) 5 (8,3) 10
fromLists :: [Gap] -> [Bin] -> State
fromLists gaps bins = case (gaps,bins) of
    (g:_  , []  ) -> End g
    ([g]  , _   ) -> End g
    (g:gs , b:bs) -> Ext g b $ fromLists gs bs
    ([]   , _   ) -> End 0

-- |Create state where all bins start as (0,0).
--
-- >>> initial [5,7,0]
-- 5 (0,0) 7 (0,0) 0
initial :: [Int] -> State
initial gs = fromLists gs $ repeat (0,0)

-- |The number of bins.
size :: State -> Int
size x = case x of
    End _ -> 0
    Ext _ _ s -> 1 + size s

-- |The bins of a state.
getBins :: State -> [Bin]
getBins x = case x of
    End _ -> []
    Ext _ b s -> b:getBins s

-- |The open values of a state.
getOpen :: State -> [Open]
getOpen x = map fst $ getBins x

-- |The open value of the Nth bin.
openN :: Index -> State -> Open
openN i x = (getOpen x)!!(i-1)

-- |The full values of a state.
getFull :: State -> [Full]
getFull x = map snd $ getBins x

-- |The full value of the Nth bin.
fullN :: Index -> State -> Full
fullN i x = (getFull x)!!(i-1)

-- |The gap values of a state.
getGap :: State -> [Gap]
getGap x = case x of
    End g -> [g]
    Ext g _ s -> g:getGap s

-- |Warning! Indexed from zero!
gapN :: Index -> State -> Gap
gapN i x = (getGap x)!!i

-- |The command @iApply i f s@ is analagous to
-- @take i s ++ f (drop i s)@.
iApply :: Trans -> Index -> Trans
iApply f index x = case (index,x) of
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
slots :: State -> Int
slots x = sum $ getGap x ++ getOpen x

-- |The probability that a state returns to itself.
probId :: Num a => State -> a
probId x = case slots x == 0 of
    True  -> 1
    False -> 0

divInt :: (Integral a, Integral b, Fractional c) => a -> b -> c
divInt x y = (fromIntegral x)/(fromIntegral y)

-- |The probability that the ith bin gains an item.
probAdd :: Fractional a => Index -> State -> a
probAdd i x = openN i x `divInt` slots x

-- |The probability that the ith bin expands to the left.
probGrowL :: Fractional a => Index -> State -> a
probGrowL i x = case test of
    True  -> 1 `divInt` slots x
    False -> 0
    where test = i == 1 || fullN i x < fullN (i-1) x

-- |The probability that the ith bin expands to the right.
probGrowR :: Fractional a => Index -> State -> a
probGrowR i x = case test of
    True  -> 1 `divInt` slots x
    False -> 0
    where test = i == size x || fullN i x <= fullN (i+1) x

---------------------------------------------------------------
-- Several functions to help study the previous process
---------------------------------------------------------------

-- |The \(l^2\) distance between a finished state
-- and a state with perfectly balanced bins.
individualLoss :: Fractional a => State -> a
individualLoss x = sum . map f . getFull $ x
    where f y = (fromIntegral y - ideal)^2
          ideal = sum (getFull x) `divInt` size x

probLoss :: Fractional a => Event a State -> a
probLoss pstate = prob pstate * (individualLoss $ event pstate)

-- |Expected loss of a set of events of @State@s.
-- Loss is the \(l^2\) distance between a finished state
-- and a state with perfectly balanced bins.
--
-- >>> expectedLoss [Event (1.0 :: Double) $ initial [1,0,3]]
-- 2.0
expectedLoss :: (Fractional a, Eq a, Markov (Event a) State) => [Event a State] -> a
expectedLoss pstates = sum . map probLoss $ (chain pstates) !! index
    where index = slots . event . head $ pstates
