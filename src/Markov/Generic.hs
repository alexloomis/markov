{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module      : Markov.Generic
Description : Realization of Markov processes with known parameters.
Maintainer  : atloomis@math.arizona.edu
Stability   : Experimental

An implementation of Markov chains allowing non-list containers.
-}

module Markov.Generic (
      Markov (..)
    ) where

import Control.Comonad (Comonad, extract)

-- |An implementation of Markov chains with non-list containers.
--
-- prop> Markov.Markov t s = Markov.Generic.Markov [] t s
class (Applicative t, Comonad t, Monad c) => Markov c t s where
    transition :: s -> c (t (s -> s))
    step       :: t s -> c (t s)
    sequential :: [s -> c (t (s -> s))]
    transition = fmap (fmap const) . step . pure
    step x = foldr ((=<<) . step') (pure x) sequential
      where step' f y = (<*> y) <$> f (extract y)
    sequential = pure transition
    {-# MINIMAL transition | step | sequential #-}
