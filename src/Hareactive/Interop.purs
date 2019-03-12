-- | This module contains functions for hooking up FRP wtih the outside world.

module Hareactive.Interop
  ( subscribe
  , producerStream
  , SinkStream
  , sinkStream
  , sinkStream'
  , sinkStreamToStream
  , pushSink
  ) where

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn1, runEffectFn2)
import Hareactive.Types (Behavior, Stream)
import Prelude (type (~>), Unit, (<$>), (<<<))

type PushCallback a = a -> Effect Unit

type ProducerFunction a = PushCallback a -> Effect (Effect Unit)

-- Create a stream from an effectful function.
producerStream :: forall a. ProducerFunction a -> Stream a
producerStream f = _producerStream (mkEffectFn1 (\cb -> f (runEffectFn1 cb)))

foreign import _producerStream :: forall a. (EffectFn1 (EffectFn1 a Unit) (Effect Unit)) -> Stream a

foreign import data SinkStream :: Type -> Type

foreign import sinkStream :: forall a. Effect (SinkStream a)

sinkStream' :: forall a. Effect (Tuple (SinkStream a) (Stream a))
sinkStream' = (\sink -> Tuple sink (sinkStreamToStream sink)) <$> sinkStream 

pushSink :: forall a. a -> SinkStream a -> Effect Unit
pushSink = runEffectFn2 _pushSink

foreign import _pushSink :: forall a. EffectFn2 a (SinkStream a) Unit

foreign import sinkStreamToStream :: SinkStream ~> Stream

-- Creates a behavior from an effectful function.
--
-- Note that while the function is allowed to be impure it should _not_ have
-- side-effects. The function should also be deteministic in the sense that it
-- always returns the same value if called at the same time. For example,
-- reading a cookie is a allowed, but returning a random number is not allowed.
fromFunction :: forall a. (Number -> Effect a) -> Behavior a
fromFunction = _fromFunction <<< mkEffectFn1

foreign import _fromFunction :: forall a. (EffectFn1 Number a) -> Behavior a

-- Executes the side-effect for each occurrence of the stream.
subscribe :: forall a. (a -> Effect Unit) -> Stream a -> Effect Unit
subscribe = runEffectFn2 _subscribe <<< mkEffectFn1

foreign import _subscribe :: forall a. EffectFn2 (EffectFn1 a Unit) (Stream a) Unit
