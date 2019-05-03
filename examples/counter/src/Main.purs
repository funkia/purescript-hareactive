module Main where

import Prelude

import Effect (Effect)
import Data.Maybe (fromJust)
import Data.Newtype (wrap)
import Web.HTML (window)
import Web.DOM.Element (Element, toEventTarget, toNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Node (setTextContent)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
import Hareactive.Types (Behavior, Stream, Now)
import Hareactive.Combinators (accum, runNow)
import Hareactive.DOM (streamFromEvent, render)
import Partial.Unsafe (unsafePartial)

-- | Just a handy (unsafe) way to get an element by its id
documentGetElementById :: String -> Effect Element
documentGetElementById id = do
  doc <- window >>= document <#> toNonElementParentNode
  melm <- getElementById id doc
  pure $ unsafePartial (fromJust melm)

-- | This is where the actual FRP logic happens, the rest is essentially just
-- | boiled plate.
createSum :: forall a b. Stream a -> Stream b -> Now (Behavior Int)
createSum incrementClick decrementClick = do
  let changes = (incrementClick $> 1) <> (decrementClick $> -1)
  accum (+) 0 changes 

main :: Effect Unit
main = do
  -- Get the DOM elements
  incrElm <- documentGetElementById "increment"
  decrElm <- documentGetElementById "decrement"
  sumElm <- documentGetElementById "sum"

  -- Convert click events into streams
  incrClick <- streamFromEvent (wrap "click") (toEventTarget incrElm)
  decrClick <- streamFromEvent (wrap "click") (toEventTarget decrElm)

  -- We then run the FRP logic
  sum <- runNow (createSum incrClick decrClick)

  -- Every time the sum changes we update the DOM
  render (\n -> setTextContent (show n) (toNode sumElm)) sum

  pure unit
