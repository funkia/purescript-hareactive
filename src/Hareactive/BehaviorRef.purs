-- | This module allows the creation of mutable behaviors. That is, behaviors
-- | whose value can be changed in an `Effect`.
-- |
-- | This module intentionally resembles
-- | [`Effect.Ref`](https://pursuit.purescript.org/packages/purescript-refs). The
-- | only difference in the API is the presence of `toBehavior` and `new'` which
-- | makes it possible to extract a `Behavior` from a `BehaviorRef`.

module Hareactive.BehaviorRef
  ( BehaviorRef
  , new
  , new'
  , toBehavior
  , read
  , modify'
  , modify
  , modify_
  , write
  ) where

-- | A `BehaviorRef` represents a behavior whose current value can be mutated
-- | similarly to a `Ref`.
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Hareactive.Types (Behavior)
import Prelude (type (~>), Unit, bind, discard, pure, void, ($), ($>), (<#>), (<$>), (>>=))

-- | A `BehaviorRef` represents a behavior whose current value can be mutated
-- | similarly to a `Ref`.
foreign import data BehaviorRef :: Type -> Type

-- | An effectful computation that creates a `BehaviorRef`. A `BehaviorRef` is a
-- | behavior that one can imperatively change the value of by using the
-- | [`writerBehavior`](#v:writerBehavior) function.
new :: forall a. a -> Effect (BehaviorRef a)
new = runEffectFn1 _new

foreign import _new :: forall a. EffectFn1 a (BehaviorRef a)

-- | This is convenience function for the common use-case of calling `new` and
-- | then also converting the `BehaviorRef` into a `Behavior` with `toBehavior`.
-- |
-- | The code
-- | ```purescript
-- | ref <- new 0
-- | behavior <- toBehavior ref
-- | ```
-- | Is equivalent to
-- | ```purescript
-- | { ref, behavior } <- new'
-- | ```
new' :: forall a. a -> Effect { ref :: BehaviorRef a, behavior :: Behavior a }
new' a = (\ref -> { ref, behavior: toBehavior ref }) <$> new a

-- | Extracts the `Behavior` from a `BehaviorRef`.
foreign import toBehavior :: BehaviorRef ~> Behavior

-- | Reads the current value of a `BehaviorRef`.
read :: forall a. BehaviorRef a -> Effect a
read = runEffectFn1 _read

foreign import _read :: forall a. EffectFn1 (BehaviorRef a) a

-- |
modify' :: forall a b. (a -> { state :: a, value :: b }) -> BehaviorRef a -> Effect b
modify' f ref = do
  value <- read ref
  let { state, value } = f value 
  write state ref
  pure value

-- |
modify :: forall s. (s -> s) -> BehaviorRef s -> Effect s
modify f ref = read ref <#> f >>= (\s -> write s ref $> s)

-- |
modify_ :: forall s. (s -> s) -> BehaviorRef s -> Effect Unit
modify_ f s = void $ modify f s

-- | Change the value of the `BehaviorRef` into the given value.
write :: forall a. a -> BehaviorRef a -> Effect Unit
write = runEffectFn2 _write

foreign import _write :: forall a. EffectFn2 a (BehaviorRef a) Unit
