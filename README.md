# Markov Tutorial

Let X<sub>n</sub> denote the nth state of a Markov chain with state space ℕ.
For x ≠ 0 define transition probabilities

p(x,0) = q,

p(x,x) = r, and

p(x,x+1) = s.

When x = 0, let
p(x,0) = q + r,
p(x,x+1) = s,
Let p(x,y) = 0 in all other cases.
Suppose we wanted to find
P\[X<sub>n</sub> = j ∩ d = k]
where d denotes the number of transitions from a positive integer to zero.
There are three values we need to track —
extinctions, probability, and state —
so we will create a type that takes three parameters.
(If we were only tracking an additive or a multiplicative parameter
along with the state, we could use `MSum` or `MProd` instead.)

```haskell
data Extinction a = Extinction { death :: Int
                               , prob  :: Rational
                               , state :: a }
                               deriving Show
```

We then need to define instances of `Extinction`
so that `Markov` knows how to handle the values we want to track.
`Extinction State` needs to be an instance of `Eq` and of `Ord`,
and `Extinction` needs to be an instance of `Applicative` and `Semigroup`.
When different paths arrive at the same state,
our instance of `Eq` determines which paths to combine,
and our instance of `Semigroup` determines how to combine them.
We want to combine paths that are at the same state
with the same number of deaths, adding their probabilities.

```haskell
instance Eq a => Eq (Extinction a) where
    x == y = state x == state y && death x == death y

instance Semigroup (Extinction a) where
    x <> y = Extinction { death = death x
                        , prob  = prob x + prob y
                        , state = state x
                        }
```

The only ordering property that matters is that equal objects
are adjacent to each other after a sort.

```haskell
instance (Ord a) => Ord (Extinction a) where
    compare x y = compare (state x) (state y)
                  `mappend`
                  compare (death x) (death y)
```

When we apply a transition, we want to add deaths
and to multiply probabilities.
This information is encoded in our instance of `Applicative`.

```haskell
instance Functor Extinction where
    fmap f (Extinction d p s) = Extinction d p (f s)

instance Applicative Extinction where
    pure x = Extinction 0 1 x
    x <*> y = Extinction { death = death x + death y
                         , prob  = prob  x * prob  y
                         , state = state x $ state y }
```

All that remains is to implement the states themselves
and make them an instance of `Markov`.

```haskell
newtype State = State Int deriving (Eq, Num, Ord, Show)

instance Markov Extinction State where
    transition x = case state x of
        State 0 -> [ Extinction 0 (q+r) id
                   , Extinction 0 s (+1) ]
        _       -> [ Extinction 1 q (\_ -> 0)
                   , Extinction 0 r id
                   , Extinction 0 s (+1) ]
        where q = 0.1; r = 0.3; s = 0.6
```

We can now easily see a list of states, deaths, and the probabilities.

__`> chain [pure (State 0) :: Extinction State] !! 3`__
```
[Extinction {death = 0, prob = 8 % 125, state = State 0}
,Extinction {death = 1, prob = 51 % 500, state = State 0}
,Extinction {death = 0, prob = 111 % 500, state = State 1}
,Extinction {death = 1, prob = 9 % 250, state = State 1}
,Extinction {death = 0, prob = 9 % 25, state = State 2}
,Extinction {death = 0, prob = 27 % 125, state = State 3}]
```

This means that starting from a state of zero,
after three time steps there is a 51/500 chance
that the state is zero and there has been one death.
