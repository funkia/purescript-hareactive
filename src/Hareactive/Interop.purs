-- | This module contains functions for connecting FRP with the rest of the
-- | world. It contains low-level functions for creating and for consuming
-- | reactives.

module Hareactive.Interop
  ( subscribe
  , ProducerFunction
  , producerStream
  , Clock
  , SinkFuture
  , sinkFuture
  , sinkFuture'
  , resolveFuture
  , sinkFutureToFuture
  , producerBehavior
  , SinkStream
  , sinkStream
  , sinkStream'
  , sinkStreamToStream
  , pushSink
  , observe
  ) where

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn3)
import Hareactive.Types (Behavior, Future, Stream)
import Prelude (type (~>), Unit, (<$>), (<<<))

-- | Hareactive represents time internally by using a logical clock. This type represents logical timestamps.
-- |
-- | This type is opaque to users of Hareactive.
foreign import data Clock :: Type

foreign import data SinkFuture :: Type -> Type

-- | Creates a future that one can resolve imperatively by calling
-- | `resolveFuture`.
foreign import sinkFuture :: forall a. Effect (SinkFuture a)

sinkFuture' :: forall a. Effect { sink :: SinkFuture a, future :: Future a }
sinkFuture' = (\sink -> { sink, future: sinkFutureToFuture sink }) <$> sinkFuture

resolveFuture :: forall a. SinkFuture a -> a -> Effect Unit
resolveFuture = runFn2 _resolveFuture

foreign import _resolveFuture :: forall a. Fn2 (SinkFuture a) a (Effect Unit)

foreign import sinkFutureToFuture :: SinkFuture ~> Future

-- | A `ProducerFunction` produces values by repeatedly invoking its first
-- | argument with new values.
type ProducerFunction a = (a -> Effect Unit) -> Effect (Effect Unit)

-- Create a stream from an effectful function.
producerStream :: forall a. ProducerFunction a -> Stream a
producerStream f = _producerStream (mkEffectFn1 (\cb -> f (runEffectFn1 cb)))

foreign import _producerStream :: forall a. (EffectFn1 (EffectFn1 a Unit) (Effect Unit)) -> Stream a

foreign import data SinkStream :: Type -> Type

-- | An effectful computation that creates a `SinkStream`. A `SinkStream` is a
-- | sink that one can imperatively push occurrences into by using the
-- | [`pushSink`](#v:pushSink) function.
foreign import sinkStream :: forall a. Effect (SinkStream a)

-- | This is a convenience function for the common use-case of calling
-- | `sinkStream` and then converting the `SinkStream` into a `Stream`
-- | with `sinkStreamToStream`.
-- |
-- | The code
-- | ```purescript
-- | sink <- sinkStream
-- | stream <- sinkStreamToStream sink
-- | ```
-- |
-- | Is equivalent to
-- | ```purescript
-- | { sink, stream } <- sinkStream'
-- | ```
sinkStream' :: forall a. Effect { sink :: SinkStream a, stream :: Stream a }
sinkStream' = (\sink -> { sink, stream: sinkStreamToStream sink }) <$> sinkStream

pushSink :: forall a. a -> SinkStream a -> Effect Unit
pushSink = runEffectFn2 _pushSink

foreign import _pushSink :: forall a. EffectFn2 a (SinkStream a) Unit

-- | Extract a `Stream` from a `SinkStream`. The resulting stream has an
-- | occurrence every time the `SinkStream` gets pushed into.
foreign import sinkStreamToStream :: SinkStream ~> Stream

-- | Creates a behavior from an effectful function.
-- |
-- | Note that while the function is allowed to be impure it should _not_ have
-- | side-effects. The function should also be deteministic in the sense that it
-- | always returns the same value if called at the same time. For example,
-- | reading a cookie is a allowed, but returning a random number is not allowed.
fromFunction :: forall a. (Number -> Effect a) -> Behavior a
fromFunction = _fromFunction <<< mkEffectFn1

foreign import _fromFunction :: forall a. (EffectFn1 Number a) -> Behavior a

-- | Create a behavior from a producer function.
producerBehavior :: forall a. ProducerFunction a -> (Clock -> Effect a) -> Behavior a
producerBehavior f getValue = runFn2 _producerBehavior (mkEffectFn1 (\cb -> f (runEffectFn1 cb))) (mkEffectFn1 getValue)

foreign import _producerBehavior :: forall a. Fn2 (EffectFn1 (EffectFn1 a Unit) (Effect Unit)) (EffectFn1 Clock a) (Behavior a)

-- | Executes the side-effect for each occurrence of the stream.
subscribe :: forall a. (a -> Effect Unit) -> Stream a -> Effect Unit
subscribe = runEffectFn2 _subscribe <<< mkEffectFn1

foreign import _subscribe :: forall a. EffectFn2 (EffectFn1 a Unit) (Stream a) Unit

-- | Executes side-effects in response to a behavior.
observe :: forall a. (a -> Effect Unit) -> (Effect Unit -> Effect (Effect Unit)) -> Behavior a -> Effect Unit
observe effect handlePull b = runEffectFn3 _observe (mkEffectFn1 effect) (mkEffectFn1 handlePull) b

foreign import _observe :: forall a. EffectFn3 (EffectFn1 a Unit) (EffectFn1 (Effect Unit) (Effect Unit)) (Behavior a) Unit
