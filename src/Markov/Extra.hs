{-# LANGUAGE FlexibleContexts           #-}
{-|
Module      : Markov.Extra
Maintainer  : atloomis@math.arizona.edu
Stability   : experimental
-}
module Markov.Extra
     ( fromLists
     , randomPath
     ) where

import Markov
import qualified Control.Monad.Random as MR
import qualified Data.List as DL

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
-- Inputs should obey:
--
-- > all (== length matrix) (map length matrix)
-- > length matrix == length states
fromLists :: Eq  b => [[a]] -> [b] -> b -> [(a, c -> b)]
fromLists matrix states b = case DL.elemIndex b states of
    Nothing -> []
    Just n  -> zip (matrix!!n) $ fmap const states
