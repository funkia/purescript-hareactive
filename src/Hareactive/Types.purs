module Hareactive.Types
  ( Behavior
  , Future
  , Stream
  , Now
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

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

-- | `append` returns the future that occurs first. The expression `a <> b` is
-- | equal to `a` if `a` occurs before `b` and `b` otherwise.
instance semigroupFuture :: Semigroup (Future a) where
  append = runFn2 _appendFuture

-- | `mempty` is a future that never occurs.
instance monoidFuture :: Monoid (Future a) where
  mempty = memptyFuture

foreign import memptyFuture :: forall a. Future a

instance functorFuture :: Functor Future where
  map = runFn2 _mapFuture

foreign import _mapFuture :: forall a b. Fn2 (a -> b) (Future a) (Future b)

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

instance functorStream :: Functor Stream where
  map = runFn2 _mapStream

foreign import _mapStream :: forall a b. Fn2 (a -> b) (Stream a) (Stream b)

--------------------------------------------------------------------------------
-- Now -------------------------------------------------------------------------
--------------------------------------------------------------------------------

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

-- | Run a `Now`-computation in the `Effect` monad.
runNow :: forall a. Now a -> Effect a
runNow = runEffectFn1 _runNow

foreign import _runNow :: forall a. EffectFn1 (Now a) a

-- | The `MonadNow` class represents all monads which support running `Now`
-- | computations.
-- |
-- | An instance is provided for all monads that support native effects. That
-- | is, any monad that implements `MonadEffect`.
class (Monad m) <= MonadNow m where
  liftNow :: forall a. Now a -> m a

instance monadNowMonadEffect :: MonadEffect m => MonadNow m where
  liftNow = liftEffect <<< runNow
