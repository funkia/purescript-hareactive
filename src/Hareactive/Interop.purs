-- | This module contains functions for connecting FRP with the rest of the
-- | world. It contains low-level functions for creating futures, streams, and
-- | behaviors "from scratch". It also contains low-level functions consuming
-- | streams and behaviors by executing side-effects in response to their
-- | occurrences and value changes.
-- |
-- | The module contains the following functions for creating reactive.
-- |
-- | * `Future`: [sinkFuture](#v:sinkFuture)
-- | * `Stream`: [sinkStream](#v:sinkStream), [producerStream](#v:producerStream)
-- | * `Behavior`: [producerBehavior](#v:producerBehavior)
-- |
-- | Since the functions in this module are low-level some of them necessitates
-- | the understanding a few details about how Hareactive is implemented. These
-- | are activation and deactivation of streams and behaviors and the difference
-- | between pull and push behaviors.
-- |
-- | #### Activation and deactivation
-- |
-- | When a stream or behavior is created in Hareactive it is _inactive_ until
-- | other circumstanses forces it to _activate_. For instance, if `s` is a
-- | stream then `map f s` creates a new stream which is inactive. Since `f` is
-- | pure and since no one depends on the occurrences of the mapped stream the
-- | occurrences are actualy never computed.
-- |
-- | As soon as something with an observable effect depends on the stream (like
-- | a stateful combinator such as `accum` or a function with side effects such
-- | as `runStreamEffect`) the stream will be activated. This is purely an
-- | optimization and it functions such that it does not leak into the rest of
-- | the API (as opposed to some other reactive libraries with similar notions
-- | that do escape into the API an affect every day users). However, when
-- | constructing streams and behaviors from scratch it is beneficial for
-- | performance reasons to understand activation/deactivation and create then
-- | such that they can activate and deactivate.
-- |
-- | For instance, if creating a stream from DOM key press events then only when
-- | the stream is activated should it add and event listener to the DOM and
-- | when the stream is deactivated it should remove the event listener from the
-- | DOM.
-- |
-- | Streams and behaviors that can activate and deactivate are created with the
-- | functions [producerStream](v:producerStream) and
-- | [producerBehavior](v:producerBehavior) respectively.
-- |
-- | ### Push and pull behaviors
-- |
-- | Behaviors in Hareactive can be in either a _push_ state or a _pull_
-- | state. When a behavior is in the push state it will notify any listeners of
-- | any changes in its value. On the other hand, when a behavior is in the pull
-- | state any listeners must ask the behavior for changes whenever it is
-- | releant for them to do so. This distinction only affects the function
-- | [observe](v:observe) which must handle behaviors in both states.
-- |

module Hareactive.Interop
  ( Clock
  , ProducerFunction
  , producerStream
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
  , subscribe
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
