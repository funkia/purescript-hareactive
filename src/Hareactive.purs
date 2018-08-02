module Hareactive
  ( Now
  , Behavior
  , Stream
  , Future
  , apply
  , filterApply
  , filter
  , filterJust
  , keepWhen
  , sample
  , snapshot
  , snapshotWith
  , scan
  , scanS
  , stepper
  , switchTo
  , switcher
  , switchStream
  , time
  , timeFrom
  , changes
  , performAff
  , runStreamAff
  ) where

import Prelude

import Data.Either (Either)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, mkFn2, runFn2, runFn3)
import Data.Maybe (Maybe, isJust, fromJust)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn2, runEffectFn1, runEffectFn2)
import Partial.Unsafe (unsafePartial)

-- Types

-- | A behavior represents a value that changes over time. I.e. a value that
-- | depends on time. Semantically a `Behavior a` can be understood as being
-- | equivalent to a function `Time -> a`. A behavior isn't implemented as a
-- | function. But the semantics serve as a mental model in terms of which all
-- | other operations can be understood.
foreign import data Behavior :: Type -> Type

-- | A future is a value associated with some point in time. Semantically a
-- | `Future a` is equivalent to `(Time, a)`. That is, a value tagged with
-- | time. In practice, `Stream` is used more often than `Future`. `Future` is
-- | useful in circumstances where some event occurs exactly once in the
-- | future.
foreign import data Future :: Type -> Type

-- | A stream represents events that occur at specific moments in time. It is a
-- | list of future values.  Semantically a `Stream a` can be understood as
-- | `List (Time, a)`. That is, a list of values where each value is associated
-- | with a specific moment in time. The time values have to be increasing.
-- | But, they do _not_ have to be _strictly_ increasing. This means that a
-- | stream can have several occurrences at the same moment in time. This can
-- | be very useful in certain circumstances.
foreign import data Stream :: Type -> Type

-- | A `Now` represents a computation that occurs at a specific moment in time.
-- | That moment is always "now". This means that computations inside the now
-- | can depend on time—but only the current point in time. The requirement
-- | that `Time` is always the current moment in time is enforced and
-- | guaranteed by the API for working with time.
-- |
-- | In addition to containing time-dependent computations a `Now` can also
-- | contain computations with side-effects. An approximate model of a `Now a`
-- | is that it is equivalent to `Time -> IO a`.
-- |
-- | `Now` servers two purposes in Hareactive: It makes it possible to create
-- | stateful behaviors that depends on the past _without_ introducing
-- | space-leaks (a notorious problem in FRP). Additionally, it is the glue
-- | between FRP primitives and effectful computations.
foreign import data Now :: Type -> Type

--------------------------------------------------------------------------------
-- Behavior --------------------------------------------------------------------
--------------------------------------------------------------------------------

foreign import _mapBehavior :: forall a b. Fn2 (a -> b) (Behavior a) (Behavior b)

instance functorBehavior :: Functor Behavior where
  map = runFn2 _mapBehavior

instance applyBehavior :: Apply Behavior where
  apply = runFn2 _applyBehavior

foreign import _applyBehavior :: forall a b. Fn2 (Behavior (a -> b)) (Behavior a) (Behavior b)

instance applicativeBehavior :: Applicative Behavior where
  pure = _pureBehavior

foreign import _pureBehavior :: forall a. a -> Behavior a

instance bindBehavior :: Bind Behavior where
  bind = runFn2 _bindBehavior

foreign import _bindBehavior :: forall a b. Fn2 (Behavior a) (a -> Behavior b) (Behavior b)

instance monadBehavior :: Monad Behavior

--------------------------------------------------------------------------------
-- Future ----------------------------------------------------------------------
--------------------------------------------------------------------------------

instance functorFuture :: Functor Future where
  map = runFn2 _mapFuture

foreign import _mapFuture :: forall a b. Fn2 (a -> b) (Future a) (Future b)

-- | The `Semigroup` instance returns the future that occurs first.
-- | The expression `a <> b` is equal to `a` if `a` occurs before `b`
-- | and `b` otherwise.
instance semigroupFuture :: Semigroup (Future a) where
  append = runFn2 _appendFuture

foreign import _appendFuture :: forall a. Fn2 (Future a) (Future a) (Future a)

instance applyFuture :: Apply Future where
  -- | Occurs once both futures have occured and applies the functions
  -- | in the former to the value in the later.
  apply = runFn2 _applyFuture

foreign import _applyFuture :: forall a b. Fn2 (Future (a -> b)) (Future a) (Future b)

instance applicativeFuture :: Applicative Future where
  pure = _pureFuture

foreign import _pureFuture :: forall a. a -> Future a

instance bindFuture :: Bind Future where
  bind = runFn2 _bindFuture

foreign import _bindFuture :: forall a b. Fn2 (Future a) (a -> Future b) (Future b)

instance monadFuture :: Monad Future

--------------------------------------------------------------------------------
-- Stream ----------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The `Semigroup` instance merges two streams by combining their occurrences
-- | while keeping them ordered with respect to time.
-- |
-- | One detail to be aware of is what happens in case both the left and the
-- | right stream contains occurences that occur simultaneously.  The
-- | `sortWith` in the semantics below is _stable_. This implies that
-- | simultaneous ocurrences in the left stream will occurr before ones in the
-- | righ stream.
-- |
-- | Semantically.
-- |
-- | ```purescript
-- | append s1 s2 = sortWith (\(time, a) -> time) (s1 <> s2)
-- | ```
instance semigroupStream :: Semigroup (Stream a) where
  append = runFn2 _combine

foreign import _combine :: forall a. Fn2 (Stream a) (Stream a) (Stream a)

-- | The `Monoid` instance lets `mempty` be a stream without any occurrences.
instance monoidStream :: Monoid (Stream a) where
  mempty = _memptyStream

foreign import _memptyStream :: forall a. Stream a

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

instance functorStream :: Functor Stream where
  map = runFn2 _mapStream

foreign import _mapStream :: forall a b. Fn2 (a -> b) (Stream a) (Stream b)

--------------------------------------------------------------------------------
-- Behavior and stream ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | Whenever the stream has an occurence the function at the behavior is
-- | applied to the value of the occurrence.
-- |
-- | Semantically.
-- | ```purescript
-- | apply b s = map (\{time, a} -> {time, a: b time a}) s
-- | ```
apply :: forall a b. Behavior (a -> b) -> Stream a -> Stream b
apply = runFn2 _apply

foreign import _apply :: forall a b. Fn2 (Behavior (a -> b)) (Stream a) (Stream b)

-- | A combination of `filter` and `apply`
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
-- | ```purescrept
-- | scan f a s =
-- |   \from, to -> foldr f a <<< map (_.a) <<< filter ({time} -> from <= time && to <= endT) $ s
-- | ```
scan :: forall a b. (a -> b -> b) -> b -> Stream a -> Behavior (Behavior b)
scan = runFn3 _scan <<< mkFn2

foreign import _scan :: forall a b. Fn3 (Fn2 a b b) b (Stream a) (Behavior (Behavior b))

scanS :: forall a b. (a -> b -> b) -> b -> Stream a -> Behavior (Stream b)
scanS = runFn3 _scanS <<< mkFn2

foreign import _scanS :: forall a b. Fn3 (Fn2 a b b) b (Stream a) (Behavior (Stream b))

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

-- | Creates a behavior whose value is the last occurrence in the stream.
stepper :: forall a. a -> Stream a -> Behavior (Behavior a)
stepper = runFn2 _stepper

foreign import _stepper :: forall a. Fn2 a (Stream a) (Behavior (Behavior a))

-- | Creates a new behavior that acts exactly like the first behavior until the
-- | future occurs after which it acts like the behavior from the future.
switchTo :: forall a. Behavior a -> Future (Behavior a) -> Behavior a
switchTo = runFn2 _switchTo

foreign import _switchTo :: forall a. Fn2 (Behavior a) (Future (Behavior a)) (Behavior a)

-- | Creates a behavior that initially acts like the first behavior and then
-- | switches to each new behavior from the stream.
switcher :: forall a. Behavior a -> Stream (Behavior a) -> Behavior (Behavior a)
switcher = runFn2 _switcher

foreign import _switcher :: forall a. Fn2 (Behavior a) (Stream (Behavior a)) (Behavior (Behavior a))

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
-- | ```purescrept
-- | timeFrom = \from, to -> to - from
-- | ```
foreign import timeFrom :: Behavior (Behavior Number)

-- | Takes a behavior and returns a stream that has an occurrence
-- | whenever the behavior changes.
foreign import changes :: forall a. Behavior a -> Stream a

--------------------------------------------------------------------------------
-- Now -------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Returns the current value of the behavior in the `Now`. This is possible
-- | because computations in the `Now` monad have an associated point in time.
foreign import sample :: forall a. Behavior a -> Now a

instance functorNow :: Functor Now where
  map = runFn2 _mapNow

foreign import _mapNow :: forall a b. Fn2 (a -> b) (Now a) (Now b)

instance applyNow :: Apply Now where
  apply = runFn2 _applyNow

foreign import _applyNow :: forall a b. Fn2 (Now (a -> b)) (Now a) (Now b)

instance applicativeNow :: Applicative Now where
  pure = _pureNow

foreign import _pureNow :: forall a. a -> Now a

instance bindNow :: Bind Now where
  bind = runFn2 _bindNow

foreign import _bindNow :: forall a b. Fn2 (Now a) (a -> Now b) (Now b)

instance monadNow :: Monad Now

-- | Runs an `Effect` inside a `Now`. The side-effect will be executed
-- | when the `Now` computation is being executed.
instance monadEffectNow :: MonadEffect Now where
  liftEffect = liftEffectNow

foreign import liftEffectNow :: forall a. Effect a -> Now a

-- | Convert a future now computation into a now computation of a future. This
-- | function is what allows a now computation to reach beyond the current
-- | moment that it is running in.
foreign import plan :: forall a. Future (Now a) -> Now (Future a)

foreign import sinkFuture :: forall a. Effect (Future a)

resolveFuture :: forall a. Future a -> a -> Effect Unit
resolveFuture = runFn2 _resolveFuture

foreign import _resolveFuture :: forall a. Fn2 (Future a) a (Effect Unit)

performAff :: forall a. Aff a -> Now (Future (Either Error a))
performAff aff = do
  future <- liftEffect sinkFuture
  liftEffect $ runAff_ (resolveFuture future) aff
  pure future

runStreamAff :: forall a. Stream (Aff a) -> Now (Stream (Either Error a))
runStreamAff s = liftEffect $ performCb (flip runAff_) s

performCb :: forall a b. (a -> (b -> Effect Unit) -> Effect Unit) -> Stream a -> Effect (Stream b)
performCb cb = runEffectFn2 _performCb (mkEffectFn2 (\a resultCb -> cb a (runEffectFn1 resultCb)))

foreign import _performCb :: forall a b. EffectFn2 (EffectFn2 a (EffectFn1 b Unit) Unit) (Stream a) (Stream b)