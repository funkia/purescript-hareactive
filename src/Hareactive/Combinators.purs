module Hareactive.Combinators
  ( applyS
  , (<~>)
  , filterApply
  , filter
  , filterJust
  , split
  , keepWhen
  , sample
  , snapshot
  , snapshotWith
  , logS
  , logB
  , scan
  , scanB
  , scanS
  , scanSB
  , stepper
  , stepperB
  , switchTo
  , switcher
  , switcherB
  , switchStream
  , time
  , timeFrom
  , changes
  , toggle
  , toggleB
  , moment
  , integrate
  , integrateB
  , performAff
  , runFutureEffect
  , runStreamEffect
  , runStreamAff
  , runNow
  ) where

import Prelude

import Data.Either (Either)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn2, runFn3)
import Data.Maybe (Maybe, isJust, fromJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2)
import Partial.Unsafe (unsafePartial)
import Hareactive.Types (Behavior, Future, Stream, Now)

-- | Filter a stream, keeping the elements which satisfy a predicate function,
-- | creating a new stream.
-- |
-- | Semantically.
-- | ```purescript
-- | filter p s = filter (\(time, a) -> p x) s
-- | ```
filter :: forall a. (a -> Boolean) -> Stream a -> Stream a
filter = runFn2 _filter

foreign import _filter :: forall a. Fn2 (a -> Boolean) (Stream a) (Stream a)

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust m = (unsafePartial (fromJust m))

-- | Removes all `Nothing` values from the stream and extracts the
-- | values from the remaining `Just`s.
filterJust :: forall a. Stream (Maybe a) -> Stream a
filterJust = map unsafeFromJust <<< filter isJust

-- | Takes a predicate and a stream. A pair of streams is returned. The first
-- | stream includes all occurrences from the original stream for which the
-- | predicate is satisfied and the seconds stream all occurrences for which the
-- | predicate is false.
-- |
-- | ```purescript
-- | Tuple smallNumbers largeNumbers = split (_ < 100) streamOfNumbers
-- | ```
split :: forall a. (a -> Boolean) -> Stream a -> Tuple (Stream a) (Stream a)
split predicate stream = unsafePartial (let [x, y] = runFn2 _split predicate stream in Tuple x y)

foreign import _split :: forall a. Fn2 (a -> Boolean) (Stream a) (Array (Stream a))

logS :: forall a. String -> Stream a -> Effect Unit
logS = runEffectFn2 _logS

foreign import _logS :: forall a. EffectFn2 String (Stream a) Unit

logB :: forall a. String -> Behavior a -> Effect Unit
logB = runEffectFn2 _logB

foreign import _logB :: forall a. EffectFn2 String (Behavior a) Unit

--------------------------------------------------------------------------------
-- Behavior and stream ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | This function is similar to `apply` for behaviors except the last argument
-- | is a stream instead of a behaviors. Whenever the stream has an occurrence
-- | the function at the behavior is applied to the value of the occurrence.
-- |
-- | This function has an operator alias `<~>`. The operator is intended to work
-- | in tandem with `<$>` and `<*>`. As an example, assume that `f3` is a
-- | function of three arguments, that `b1` and `b2` are two behaviors, and that
-- | `s` is a stream.` Then the function can be applied to the two behaviors and
-- | the stream in the following way.
-- |
-- | ```purescript
-- | f3 <$> b1 <*> b2 <~> s
-- | ```
-- |
-- | With the above code, whenever `s` has an occurrence the value of `b1`,
-- | `b2`, and the value of the occurrence will be applied to `f3` and its
-- | return value will be the value of the occurrence in the resulting stream.
-- |
-- | Semantically.
-- | ```purescript
-- | applyS b s = map (\{time, a} -> {time, a: (b time) a}) s
-- | ```
applyS :: forall a b. Behavior (a -> b) -> Stream a -> Stream b
applyS = runFn2 _applyS

foreign import _applyS :: forall a b. Fn2 (Behavior (a -> b)) (Stream a) (Stream b)

infixl 4 apply as <~>

-- | A combination of `filter` and `apply`. For each occurrence of the stream
-- | the predicate at the behavior at that time is applied to the value and the
-- | returned stream contains the occurrence if and only if the predicate
-- | returns true.
-- |
-- | This function can be seen as a generalization of `filter`. Where `filter`
-- | takes a constant predicate function `filterApply` takes a varying predicate
-- | in the form of a behavior of a predicate. As such `filterApply (pure
-- | predicate) stream` is equivalent to `filter predicate stream`.
filterApply :: forall a. Behavior (a -> Boolean) -> Stream a -> Stream a
filterApply = runFn2 _filterApply

foreign import _filterApply :: forall a. Fn2 (Behavior (a -> Boolean)) (Stream a) (Stream a)

-- | Filter a stream, keeping the elements which satisfy a predicate function,
-- | creating a new stream.
-- |
-- | ```purescrept
-- | keepWhen s b = filter (\{time, a} -> b time) s
-- | ```
keepWhen :: forall a. Stream a -> Behavior Boolean -> Stream a
keepWhen = runFn2 _keepWhen

foreign import _keepWhen :: forall a. Fn2 (Stream a) (Behavior Boolean) (Stream a)

-- | For each occurrence on the stream the function is applied to the value and
-- | the accumulator.
-- |
scan :: forall a b. (a -> b -> b) -> b -> Stream a -> Now (Behavior b)
scan f init s = sample $ scanB f init s

-- | Generalization of `scan` satisfying the equation `scan f init s = sample $ scanB f init s`.
-- |
-- | Semantically.
-- | ```purescrept
-- | scan f a s =
-- |   \from, to -> foldr f a <<< map (_.a) <<< filter ({time} -> from <= time && to <= endT) $ s
-- | ```
scanB :: forall a b. (a -> b -> b) -> b -> Stream a -> Behavior (Behavior b)
scanB = runFn3 _scanB <<< mkFn2

foreign import _scanB :: forall a b. Fn3 (Fn2 a b b) b (Stream a) (Behavior (Behavior b))

-- | Similar to `scan` but instead of returning a behavior it returns a stream.
-- |
scanS :: forall a b. (a -> b -> b) -> b -> Stream a -> Now (Stream b)
scanS f init s = sample $ scanSB f init s

-- | Generalization of `scanS` satisfying the equation `scanS f init s = sample $ scanSB f init s`.
scanSB :: forall a b. (a -> b -> b) -> b -> Stream a -> Behavior (Stream b)
scanSB = runFn3 _scanSB <<< mkFn2

foreign import _scanSB :: forall a b. Fn3 (Fn2 a b b) b (Stream a) (Behavior (Stream b))

-- | Creates a behavior whose value is the last occurrence in the stream.
stepper :: forall a. a -> Stream a -> Now (Behavior a)
stepper init s = sample $ stepperB init s

-- | Generalization of `stepper` satisfying `stepper init s = sample $ stepperB
-- | init s`.
stepperB :: forall a. a -> Stream a -> Behavior (Behavior a)
stepperB = runFn2 _stepperB

foreign import _stepperB :: forall a. Fn2 a (Stream a) (Behavior (Behavior a))

-- | Creates a stream that occurs exactly when the given stream occurs. Every
-- | time the stream s has an occurrence the current value of the behavior is
-- | sampled. The value in the occurrence is then replaced with the sampled
-- | value.
snapshot :: forall a b. Behavior a -> Stream b -> Stream a
snapshot = runFn2 _snapshot

foreign import _snapshot :: forall a b. Fn2 (Behavior a) (Stream b) (Stream a)

-- | Returns a stream that occurs whenever the given stream occurs. At each
-- | occurrence the value and the value from the behavior is passed to the
-- | function and the return value is the value of the returned streams
-- | occurrence.
snapshotWith :: forall a b c. (a -> b -> c) -> Behavior b -> Stream a -> Stream c
snapshotWith f b a = runFn3 _snapshotWith (mkFn2 f) b a

foreign import _snapshotWith :: forall a b c. Fn3 (Fn2 a b c) (Behavior b) (Stream a) (Stream c)

-- | Creates a new behavior that acts exactly like the first behavior until the
-- | future occurs after which it acts like the behavior from the future.
switchTo :: forall a. Behavior a -> Future (Behavior a) -> Behavior a
switchTo = runFn2 _switchTo

foreign import _switchTo :: forall a. Fn2 (Behavior a) (Future (Behavior a)) (Behavior a)

-- | Creates a behavior that initially acts like the first behavior and then
-- | switches to each new behavior from the stream.
-- |
-- | This function is equal to `switcherB >>> sample`
switcher :: forall a. Behavior a -> Stream (Behavior a) -> Now (Behavior a)
switcher b s = sample $ switcherB b s

-- | Creates a behavior that initially acts like the first behavior and then
-- | switches to each new behavior from the stream.
switcherB :: forall a. Behavior a -> Stream (Behavior a) -> Behavior (Behavior a)
switcherB = runFn2 _switcherB

foreign import _switcherB :: forall a. Fn2 (Behavior a) (Stream (Behavior a)) (Behavior (Behavior a))

-- | Takes a stream valued behavior and returns a stream that emits values from
-- | the current stream at the behavior. I.e. the returned stream always
-- | "switches" to the current stream at the behavior.
foreign import switchStream :: forall a. Behavior (Stream a) -> Stream a

-- | A behavior whose value is the number of milliseconds elapsed since UNIX
-- | epoch.
foreign import time :: Behavior Number

-- | A behavior giving access to continous time. When sampled the outer
-- | behavior returns a behavior whose value is the time since the outer
-- | behavior was sampled.
-- |
-- | Semantically.
-- | ```purescript
-- | timeFrom = \from, to -> to - from
-- | ```
foreign import timeFrom :: Behavior (Behavior Number)

-- | Takes a behavior and returns a stream that has an occurrence
-- | whenever the behavior changes.
changes :: forall a. Eq a => Behavior a -> Stream a
changes b = runFn2 _changes b (mkFn2 (==))

foreign import _changes :: forall a. Fn2 (Behavior a) (Fn2 a a Boolean) (Stream a)

-- | Creates a behavior that switches between `true` and `false`. Initally it
-- | takes the value of its first argument. Each occurrence of the first stream
-- | will make the behavior `true` and each occurrence of the second stream
-- | makes the behavior `false`.
-- |
-- | The example below demonstrates one use case for `toggle`. A stream
-- | `doorOpen` signifies that a door has been opened and similairly a stream
-- | `doorClose` signifies that the door has closed. `toggle` is then used to
-- | construct a behavior that at any time represents the state of the door.
-- |
-- | ```purescript
-- | isDoorOpen <- sample $ toggle false doorOpen doorClose
-- | ```
toggle :: forall a b. Boolean -> Stream a -> Stream b -> Now (Behavior Boolean)
toggle initial on off = sample $ toggleB initial on off

-- | Generalization of `toggle` satisfying `toggle initial on off = sample $
-- | toggleB initial on off`.
toggleB :: forall a b. Boolean -> Stream a -> Stream b -> Behavior (Behavior Boolean)
toggleB = runFn3 _toggleB

foreign import _toggleB :: forall a b. Fn3 Boolean (Stream a) (Stream b) (Behavior (Behavior Boolean))

foreign import moment :: forall b. ((forall a. Behavior a -> a) -> b) -> Behavior b

-- | Integrate behavior with respect to time. The value of the given behavior is
-- | interpreted as being a rate of change _per second_.
-- |
-- | Note that `integrate` is implemented using Euler's method. Hence the
-- | resulting behavior is not exact but includes some numerical error.
integrate :: Behavior Number -> Now (Behavior Number)
integrate = sample <<< integrateB

-- | Generalization of `integrate` satisfying `integrate = sample <<<
-- | integrateB`.
foreign import integrateB :: Behavior Number -> Behavior (Behavior Number)

--------------------------------------------------------------------------------
-- Now -------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Returns the current value of the behavior in the `Now`. This is possible
-- | because computations in the `Now` monad have an associated point in time.
foreign import sample :: forall a. Behavior a -> Now a

-- | Convert a future now computation into a now computation of a future. This
-- | function is what allows a now computation to reach beyond the current
-- | moment that it is running in.
foreign import plan :: forall a. Future (Now a) -> Now (Future a)

foreign import sinkFuture :: forall a. Effect (Future a)

resolveFuture :: forall a. Future a -> a -> Effect Unit
resolveFuture = runFn2 _resolveFuture

foreign import _resolveFuture :: forall a. Fn2 (Future a) a (Effect Unit)

-- | Takes a stream of `Aff` and runs each side-effect. The returned stream has
-- | an occurrence for the result from each asynchronous computation.
performAff :: forall a. Aff a -> Now (Future (Either Error a))
performAff aff = do
  future <- liftEffect sinkFuture
  liftEffect $ runAff_ (resolveFuture future) aff
  pure future

-- | Takes a future effect and returns a now-computation that runs the effect
-- | once the future occurs and delivers the result in a future.
runFutureEffect :: forall a. Future (Effect a) -> Now (Future a)
runFutureEffect s = runFn2 _performMapFuture (mkEffectFn1 \a -> a) s

-- | Takes a stream of effects and returns a now-computation that runs the
-- | effect in each occurrence and delivers the result in a stream.
runStreamEffect :: forall a. Stream (Effect a) -> Now (Stream a)
runStreamEffect s = runFn2 _performMap (mkEffectFn1 \a -> a) s

runStreamAff :: forall a. Stream (Aff a) -> Now (Stream (Either Error a))
runStreamAff s = liftEffect $ mapCbStream (flip runAff_) s

mapCbStream :: forall a b. (a -> (b -> Effect Unit) -> Effect Unit) -> Stream a -> Effect (Stream b)
mapCbStream cb = runEffectFn2 _mapCbStream (mkEffectFn2 (\a resultCb -> cb a (runEffectFn1 resultCb)))

foreign import _mapCbStream :: forall a b. EffectFn2 (EffectFn2 a (EffectFn1 b Unit) Unit) (Stream a) (Stream b)

-- | Returns an `Effect` that executes the `Now` computation.
runNow :: forall a. Now a -> Effect a
runNow = runEffectFn1 _runNow

foreign import _runNow :: forall a. EffectFn1 (Now a) a

foreign import _performMap :: forall a b. Fn2 (EffectFn1 a b) (Stream a) (Now (Stream b))

foreign import _performMapFuture :: forall a b. Fn2 (EffectFn1 a b) (Future a) (Now (Future b))
