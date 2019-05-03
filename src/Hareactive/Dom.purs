-- | This module contains functions for interacting with DOM.

module Hareactive.DOM
  ( streamFromEvent
  , keyDown
  , keyUp
  , keyPressed
  , render
  ) where

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn2)
import Hareactive.Types (Behavior, Now, Stream)
import Prelude (Unit)
import Web.Event.Event (EventType(..), Event)
import Web.Event.EventTarget (EventTarget)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

-- | Creates a stream from a DOM element and an event type. You can think of
-- | this as the FRP equivalent of
-- | [addEventListener](https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener).
-- |
-- | ```purescript
-- | clickStream <- streamFromEvent (wrap "click") (toEventTarget buttonElement)
-- | ```
streamFromEvent :: EventType -> EventTarget -> Effect (Stream Event)
streamFromEvent (EventType s) t = runEffectFn2 _streamFromEvent t s

foreign import _streamFromEvent :: EffectFn2 EventTarget String (Stream Event)

-- | Returns a stream that has an occurrence whenever a key is pressed down. The
-- | value is the
-- | [KeyboardEvent](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent)
-- | associated with the key press.
foreign import keyDown :: Stream KeyboardEvent

-- | Returns a stream that has an occurrence whenever a key is released. The
-- | value is the
-- | [KeyboardEvent](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent)
-- | associated with the key press.
foreign import keyUp :: Stream KeyboardEvent

-- | Returns a behavior that is true when the key is pressed and false then the
-- | key is not pressed.
-- |
-- | The string specifying the key is a
-- | [KeyboardEvent.code](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/code).
foreign import keyPressed :: String -> Now (Behavior Boolean)

-- | Render the value of a behavior with an effectful render function.
-- |
-- | The render function is called on each frame using `requestAnimationFrame`
-- | if the behavior has changed.
render :: forall a. (a -> Effect Unit) -> Behavior a -> Effect Unit
render cb = runEffectFn2 _render (mkEffectFn1 cb)

foreign import _render :: forall a. EffectFn2 (EffectFn1 a Unit) (Behavior a) Unit
