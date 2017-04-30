module Run
  ( Run
  , run
  , runBase
  , interpret
  , liftEffect
  , liftBase
  , peel
  , send
  , BaseEff
  , BaseAff
  , module Data.Functor.Variant
  , module Exports
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Free (Free, liftF, runFree, foldFree, hoistFree, resume)
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Data.Either (Either)
import Data.Functor.Variant (VariantF, FProxy(..), inj, on, case_, default)
import Data.Newtype (class Newtype, unwrap, over)
import Data.Symbol (SProxy(..), class IsSymbol)
import Data.Symbol (SProxy(..)) as Exports
import Partial.Unsafe (unsafeCrashWith)
import Type.Equality (class TypeEquals)
import Unsafe.Coerce (unsafeCoerce)

-- | An extensible effect Monad, indexed by a set of effect functors. Effects
-- | are eliminated by interpretation into a pure value or into some base
-- | effect Monad. The `Run` Monad is an alternative to Monad Transformers,
-- | and can represent both associative and non-associative effects.
-- |
-- | An example using `State` and `Except`:
-- | ```purescript
-- | type MyEffects =
-- |   ( state ∷ STATE Int
-- |   , except ∷ EXCEPT String
-- |   , base ∷ BaseEff (console ∷ CONSOLE)
-- |   )
-- |
-- | yesProgram ∷ Run MyEffects Unit
-- | yesProgram = do
-- |   whenM (gets (_ < 0)) do
-- |     throw "Number is less than 0"
-- |   whileM_ (gets (_ > 0)) do
-- |     liftEff $ log "Yes"
-- |     modify (_ - 1)
-- |
-- | main =
-- |   yesProgram
-- |     # catch (liftEff <<< log)
-- |     # runState 10
-- |     # runBase
-- |     # void
-- | ````
newtype Run (r ∷ # Type) a = RunM (Free (VariantF r) a)

derive instance newtypeRun ∷ Newtype (Run r a) _
derive newtype instance functorRun :: Functor (Run r)
derive newtype instance applyRun :: Apply (Run r)
derive newtype instance applicativeRun :: Applicative (Run r)
derive newtype instance bindRun :: Bind (Run r)
derive newtype instance monadRun :: Monad (Run r)

-- | Lifts an effect functor into the `Run` Monad according to the provided
-- | `SProxy` slot.
liftEffect
  ∷ ∀ sym r1 r2 f a
  . RowCons sym (FProxy f) r1 r2
  ⇒ IsSymbol sym
  ⇒ Functor f
  ⇒ SProxy sym
  → f a
  → Run r2 a
liftEffect p = RunM <<< liftF <<< inj p

-- | Lifts a base effect into the `Run` Monad (eg. `Eff`, `Aff`, or `IO`).
liftBase
  ∷ ∀ r f a
  . Functor f
  ⇒ f a
  → Run (base ∷ FProxy f | r) a
liftBase = RunM <<< liftF <<< inj (SProxy ∷ SProxy "base")

-- | Reflects the next instruction or the final value if there are no more
-- | instructions.
peel
  ∷ ∀ a r
  . Run r a
  → Either (VariantF r (Run r a)) a
peel (RunM r) = coerceR (resume r)
  where
  coerceR ∷ Either (VariantF r (Free (VariantF r) a)) a → Either (VariantF r (Run r a)) a
  coerceR = unsafeCoerce

-- | Enqueues an instruction in the `Run` Monad.
send
  ∷ ∀ a r
  . VariantF r a
  → Run r a
send = RunM <<< liftF

-- | Extracts the value from a fully interpreted program.
run ∷ ∀ a. Run () a → a
run = unwrap >>> runFree \_ → unsafeCrashWith "Control.Monad.Run: the impossible happened"

-- | Extracts the value from a program with only base effects.
runBase
  ∷ ∀ f a
  . MonadRec f
  ⇒ Run (base ∷ FProxy f) a
  → f a
runBase = unwrap >>> foldFree go
  where
  go ∷ VariantF (base ∷ FProxy f) ~> f
  go = case_ # on (SProxy ∷ SProxy "base") id

-- | Interprets an effect functor into the base effect, eliminating the label.
interpret
  ∷ ∀ sym f m r1 r2 r3 a
  . RowCons sym (FProxy f) r2 r1
  ⇒ RowCons "base" (FProxy m) r2 r3
  ⇒ IsSymbol sym
  ⇒ Functor m
  ⇒ SProxy sym
  → (f ~> m)
  → Run r1 a
  → Run r3 a
interpret p k = over RunM (hoistFree go)
  where
  go ∷ VariantF r1 ~> VariantF r3
  go = on p (coerceB <<< inj (SProxy ∷ SProxy "base") <<< k) coerceR

  coerceB ∷ VariantF (base ∷ FProxy m | r2) ~> VariantF r3
  coerceB = unsafeCoerce

  coerceR ∷ VariantF r2 ~> VariantF r3
  coerceR = unsafeCoerce

data R (r ∷ # Type)

-- Type synonym for using `Eff` as a base effect.
type BaseEff eff = FProxy (Eff eff)

-- Type synonym for using `Aff` as a base effect.
type BaseAff eff = FProxy (Aff eff)

instance monadRecRun ∷ MonadRec (Run r) where
  tailRecM f = loop
    where
    loop a = do
      b ← f a
      case b of
        Done r → pure r
        Loop n → loop n

instance monadEffRun ∷ (TypeEquals (R rs) (R (base ∷ FProxy m | r)), MonadEff eff m) ⇒ MonadEff eff (Run rs) where
  liftEff = coerceR <<< liftBase <<< liftEff
    where
    coerceR ∷ Run (base ∷ FProxy m | r) ~> Run rs
    coerceR = unsafeCoerce

instance monadAffRun ∷ (TypeEquals (R rs) (R (base ∷ FProxy m | r)), MonadAff eff m) ⇒ MonadAff eff (Run rs) where
  liftAff = coerceR <<< liftBase <<< liftAff
    where
    coerceR ∷ Run (base ∷ FProxy m | r) ~> Run rs
    coerceR = unsafeCoerce
