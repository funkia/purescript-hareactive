module Test.Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Hareactive.Interop (pushSink, sinkStream', sinkStreamToStream, subscribe)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

addRef :: Int -> Ref.Ref Int -> Effect Unit
addRef n ref = Ref.modify_ (n + _) ref

main :: Effect Unit
main = run [consoleReporter] do
  describe "hareactive" do
    it "sink and subscribe works" do
      n <- liftEffect $ Ref.new 0
      Tuple sink stream <- liftEffect $ sinkStream'
      liftEffect $ subscribe (_ `addRef` n) stream
      liftEffect $ pushSink 3 sink
      liftEffect $ pushSink 2 sink
      liftEffect $ pushSink 5 sink
      result <- liftEffect $ Ref.read n
      result `shouldEqual` 10
      pure unit
