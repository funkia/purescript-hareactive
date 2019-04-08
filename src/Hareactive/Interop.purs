-- | This module contains functions for connecting FRP wtih the outside world.
-- | It low-level functions for creating and for consuming reactives.

module Hareactive.Interop
  ( subscribe
  , producerStream
  , Clock
  , producerBehavior
  , SinkStream
  , sinkStream
  , sinkStream'
  , sinkStreamToStream
  , pushSink
  , MutableBehavior
  , mutableBehavior
  , mutableBehavior'
  , mutableBehaviorToBehavior
  , writerBehavior
  , readBehavior
  , observe
  ) where

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn3)
import Hareactive.Types (Behavior, Stream)
import Prelude (type (~>), Unit, (<$>), (<<<))

-- | Hareactive represents time internally by using a logical clock. This type represents logical timestamps.
-- |
-- | This type is opaque to users of Hareactive.
foreign import data Clock :: Type

type PushCallback a = a -> Effect Unit

type ProducerFunction a = PushCallback a -> Effect (Effect Unit)

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
-- | Tuple sink stream <- sinkStream'
-- | ```
sinkStream' :: forall a. Effect (Tuple (SinkStream a) (Stream a))
sinkStream' = (\sink -> Tuple sink (sinkStreamToStream sink)) <$> sinkStream

pushSink :: forall a. a -> SinkStream a -> Effect Unit
pushSink = runEffectFn2 _pushSink

foreign import _pushSink :: forall a. EffectFn2 a (SinkStream a) Unit

foreign import sinkStreamToStream :: SinkStream ~> Stream

foreign import data SinkBehavior :: Type -> Type

-- | An effectful computation that creates a `MutableBehavior`. A
-- | `MutableBehavior` is a behavior that one can imperatively change the value
-- | of by using the [`writerBehavior`](#v:writerBehavior) function.
foreign import mutableBehavior :: forall a. Effect (MutableBehavior a)

-- | This is convenience function for the common use-case of calling
-- | `mutableBehavior` and then also converting the `MutableBehavior` into a `Behavior`
-- | with `mutableBehaviorToBehavior`.
-- |
-- | The code
-- | ```purescript
-- | mutable <- mutableBehavior
-- | stream <- mutableBehaviorToBehavior mutable
-- | ```
-- |
-- | Is equivalent to
-- | ```purescript
-- | Tuple mutable stream <- mutableBehavior'
-- | ```
mutableBehavior' :: forall a. Effect (Tuple (MutableBehavior a) (Behavior a))
mutableBehavior' = (\mutable -> Tuple mutable (mutableBehaviorToBehavior mutable)) <$> mutableBehavior

writerBehavior :: forall a. a -> MutableBehavior a -> Effect Unit
writerBehavior = runEffectFn2 _writerBehavior

foreign import _writerBehavior :: forall a. EffectFn2 a (MutableBehavior a) Unit

foreign import mutableBehaviorToBehavior :: MutableBehavior ~> Behavior

foreign import readBehavior :: forall a. Behavior a -> Effect a

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
