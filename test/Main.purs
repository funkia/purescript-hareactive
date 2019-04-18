module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Hareactive.BehaviorRef as BRef
import Hareactive.Interop (pushSink, sinkStream', subscribe)
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
      { sink, stream } <- liftEffect $ sinkStream'
      liftEffect $ subscribe (_ `addRef` n) stream
      liftEffect $ pushSink 3 sink
      liftEffect $ pushSink 2 sink
      liftEffect $ pushSink 5 sink
      result <- liftEffect $ Ref.read n
      result `shouldEqual` 10
      pure unit
    describe "subscribe" do
      it "can subscribe to stream" do
        result <-
          liftEffect do
            n <- Ref.new 0
            { sink, stream } <- sinkStream'
            subscribe (_ `addRef` n) stream
            pushSink 3 sink
            pushSink 2 sink
            pushSink 5 sink
            Ref.read n
        result `shouldEqual` 10
     -- describe "observe" $ do
     --   it "can observe sink" $ liftEffect do
     --     n <- Ref.new 0
     --     { sink, stream } <- mutableBehavior'
     --     observe (_ `addRef` n) (\_ -> pure unit) stream
     --     pushSink 3 sink
     --     pushSink 2 sink
     --     pushSink 5 sink
     --     result <- liftEffect $ Ref.read n
     --     result `shouldEqual` 10
    describe "BehaviorRef" do
      it "works as a reference" do
        ref <- liftEffect $ BRef.new 3
        cur1 <- liftEffect $ BRef.read ref
        cur1 `shouldEqual` 3
        liftEffect $ liftEffect $ BRef.modify_ (_ * 3) ref
        cur2 <- liftEffect $ BRef.read ref
        cur2 `shouldEqual` 9
