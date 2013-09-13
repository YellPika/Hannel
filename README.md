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
* `sync :: Event a -> IO a` blocks the calling thread until the given event returns a value.
* `return :: a -> Event a` creates an event that can always be synchronized on. It returns the supplied value. Also known as `pure`.
* `mzero :: Event a` is an event that can never be synchronized on. Also known as `empty`.
* `mplus :: Event a -> Event a -> Event a` creates an event that can nondeterministically synchronize on either of its arguments. Also known as the `<|>` operator.
* `(>>=) :: Event a -> (a -> Event b) -> Event b` sequences two events. The second event is determined from the value of the first.
* `merge :: [Event a] -> Event [a]` combines the results of a list of events. Unlike `sequence`, the order of the events does not matter, which can prevent deadlocks.
* `swap :: Event (a -> Event b, b -> Event a)` is an event that returns a pair of functions when synchronized. These functions create a pair of events which may be used to swap values from different threads.

The `Event` type implements a `MonadPlus`, so it has all the operations available to it that are normally available to that typeclass. One function of particular interest may be the `msum` function, which will allow choice over a list of events.

*Example*:
```haskell
import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Hannel.Event as Event

main :: IO ()
main = do
  (send, recieve) <- Event.sync Event.swap
  
  forkIO $ do
    putStrLn "Sending..."
    Event.sync $ send "Hello"
    putStrLn "Sent"

  putStrLn "Receiving..."
  message <- Event.sync $ receive ()
  putStrLn $ "Received: " ++ message
```

Possible output:
```
Sending...
Receiving...
Sent
Received: Hello World
```

*TODO:*
- [ ] Implement proper fairness. Choice is random, but is biased towards the rightmost event in the case where the number of choices is greater than two.
- [ ] Implement `fork :: IO () -> Event ThreadId`. When synchronized, this event forks the specified action on a new thread. This could be generalized to something like CML's `wrap :: Event a -> (a -> IO ()) -> Event a`, which specifies a post synchronization action.
- [ ] Implement proper exception handling.
- [ ] Implement different kinds of channels.
