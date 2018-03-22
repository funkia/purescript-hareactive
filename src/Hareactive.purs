module Data.Hareactive
  ( Now
  , Behavior
  , Stream
  , filter
  , keepWhen
  ) where

import Prelude (class Semigroup, class Functor, (<<<), class Apply, class Applicative, class Bind, class Monad)
import Control.Monad.Eff (kind Effect)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn2, runFn3)

-- Types

foreign import data FRP :: Effect

foreign import data Now :: Type -> Type

foreign import data Behavior :: Type -> Type

foreign import data Stream :: Type -> Type

--------------------------------------------------------------------------------
-- Stream ----------------------------------------------------------------------
--------------------------------------------------------------------------------

instance semigroupStream :: Semigroup (Stream a) where
  append = runFn2 _combine

foreign import _combine :: forall a. Fn2 (Stream a) (Stream a) (Stream a)

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

