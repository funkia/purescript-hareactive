-- | This module contains functions for interacting with DOM.

module Hareactive.Dom
  ( streamFromEvent
  ) where

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Hareactive.Types (Stream)
import Web.Event.Event (EventType(..), Event)
import Web.Event.EventTarget (EventTarget)

streamFromEvent :: EventType -> EventTarget -> Effect (Stream Event)
streamFromEvent (EventType s) t = runEffectFn2 _streamFromEvent t s

foreign import _streamFromEvent :: EffectFn2 EventTarget String (Stream Event)
