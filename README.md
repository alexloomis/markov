# Markov Tutorial

Let Xₙ denote the nth state of a Markov chain with state space ℕ.
For x ≠ 0 define transition probabilities

p(x,0) = q,

p(x,x) = r, and

p(x,x+1) = s.

When x = 0, let
p(x,0) = q+r,
p(x,x+1) = s.
Let p(x,y) = 0 in all other cases.
Suppose we wanted to find
P\[Xₙ = j and d = k],
where d denotes the number of transitions from a positive integer to zero.
There are three values we need to track —
extinctions, probability, and state.
Extinctions add a value to a counter each time they happen
and the counter takes integral values,
so they can be represented by `Sum Int`.
Probabilities are multiplied each step,
and added when duplicate steps are combined.
We want decimal probabilities, so
we can represent this with `Product Rational`.
We will make a new type for the state.

```haskell
newtype Extinction = Extinction Int
    deriving newtype (Eq, Num, Ord, Show)
```

Combining identical states should not change the state,
so we make an instance of `Combine` as follows.

```haskell
instance Combine Extinction where combine = const
```

All that remains is to make an instance of `Markov`.

```haskell
instance Markov ((,) (Sum Int, Product Rational)) Extinction where
    transition = \case
        0 -> [ 0 >*< (q+r) >*< id
             , 0 >*< s >*< (+1) ]
        _ -> [ 1 >*< q >*< const 0
             , 0 >*< r >*< id
             , 0 >*< s >*< (+1) ]
        where q = 0.1; r = 0.3; s = 0.6
```

We can now see a list of states, extinctions, and the probabilities.

__`> chain [pure 0 :: Sum Int :* Product Rational :* Extinction] !! 3`__
```
[ ((0,8 % 125),0)
, ((0,111 % 500),1)
, ((1,51 % 500),0)
, ((0,9 % 25),2)
, ((1,9 % 250),1)
, ((0,27 % 125),3) ]
```

This means that starting from a state of zero,
after three time steps there is a 51/500 chance
that the state is zero and there has been one extinction.

