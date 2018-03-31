module Data.Hareactive
  ( Now
  , Behavior
  , Stream
  , filter
  , keepWhen
  , sample
  , scan
  , scanS
  , switchStream
  ) where

import Prelude (class Semigroup, class Functor, (<<<), class Apply, class Applicative, class Bind, class Monad)
import Data.Monoid (class Monoid)
import Control.Monad.Eff (kind Effect)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn2, runFn3)

-- Types

foreign import data FRP :: Effect

-- | A behavior represents a value that changes over time. I.e. a value that
-- | depends on time. Semantically a `Behavior a` can be understood as being
-- | equivalent to a function `Time -> a`. A behavior isn't implemented as a
-- | function. But the semantics serve as a mental model in terms of which all
-- | other operations can be explained.
foreign import data Behavior :: Type -> Type

-- | A stream represents events that occur at specific moments in time.
-- | Semantically a `Stream a` can be understood as `List (Time, a)`. That is,
-- | a list of values where each value is associated with a specific moment in
-- | time. The time values have to be increasing. But, they do _not_ have to be
-- | _strictly_ increasing. This means that a stream can have several
-- | occurrences at the same moment in time. This can be very useful in certain
-- | circumstances.
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
-- Stream ----------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The semigroup instance of Stream.
instance semigroupStream :: Semigroup (Stream a) where
  append = runFn2 _combine

foreign import _combine :: forall a. Fn2 (Stream a) (Stream a) (Stream a)

instance monoidStream :: Monoid (Stream a) where
  mempty = _memptyStream

foreign import _memptyStream :: forall a. Stream a

-- | Filter a stream, keeping the elements which satisfy a predicate function,
-- | creating a new stream.
filter :: forall a. (a -> Boolean) -> Stream a -> Stream a
filter = runFn2 _filter

foreign import _filter :: forall a. Fn2 (a -> Boolean) (Stream a) (Stream a)

instance functorStream :: Functor Stream where
  map = runFn2 _mapStream

foreign import _mapStream :: forall a b. Fn2 (a -> b) (Stream a) (Stream b)

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
-- Behavior and stream ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | Filter a stream, keeping the elements which satisfy a predicate function,
-- | creating a new stream.
keepWhen :: forall a. Stream a -> Behavior Boolean -> Stream a
keepWhen = runFn2 _keepWhen

foreign import _keepWhen :: forall a. Fn2 (Stream a) (Behavior Boolean) (Stream a)

scan :: forall a b. (a -> b -> b) -> b -> Stream a -> Behavior (Behavior b)
scan = runFn3 _scan <<< mkFn2

foreign import _scan :: forall a b. Fn3 (Fn2 a b b) b (Stream a) (Behavior (Behavior b))

scanS :: forall a b. (a -> b -> b) -> b -> Stream a -> Behavior (Stream b)
scanS = runFn3 _scanS <<< mkFn2

foreign import _scanS :: forall a b. Fn3 (Fn2 a b b) b (Stream a) (Behavior (Stream b))

foreign import switchStream :: forall a. Behavior (Stream a) -> Stream a

--------------------------------------------------------------------------------
-- Now -------------------------------------------------------------------------
--------------------------------------------------------------------------------

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

