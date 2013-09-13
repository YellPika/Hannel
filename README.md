Hannel
======

A lock based implementation of [transactional events](http://www.cs.rit.edu/~mtf/research/tx-events/JFP08/jfp08.pdf).

At the core of Hannel is the `Event` type:
```haskell
type Event a

instance Monad a
instance MonadPlus a
instance Applicative a
instance Functor a
instance Alternative a
```

The `Event` type has the following operations:
* `return :: a -> Event a` or `pure :: a -> Event a`: Creates an event that can always be synchronized on, and returns the supplied value.
* `mzero :: Event a` or `empty :: Event a`: An attempt to synchronize on this event will never return.
* `mplus :: Event a -> Event a -> Event a``(<|>) :: Event a -> Event a -> Event a`: Chooses between two events.
