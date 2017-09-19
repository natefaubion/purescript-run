module Run
  ( Run
  , run
  , runBase
  , runBaseWithCont
  , runWithBase
  , runWithEffect
  , liftEffect
  , liftBase
  , peel
  , send
  , expand
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
import Control.Monad.Free (Free, liftF, runFree, foldFree, resume')
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Data.Either (Either(..))
import Data.Functor.Variant (VariantF, FProxy(..), inj, on, case_, default)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..)) as Exports
import Data.Symbol (SProxy(..), class IsSymbol)
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
peel = unwrap >>> resume' (\b f → Left (RunM <<< f <$> b)) Right

-- | Enqueues an instruction in the `Run` Monad.
send
  ∷ ∀ a r
  . VariantF r a
  → Run r a
send = RunM <<< liftF

-- | Casts some set of effects to a wider set of effects via a left-biased
-- | union.
expand
  ∷ ∀ r1 r2 rx a
  . Union r1 rx r2
  ⇒ Run r1 a
  → Run r2 a
expand = unsafeCoerce

-- | Extracts the value from a fully interpreted program.
run ∷ ∀ a. Run () a → a
run = unwrap >>> runFree \_ → unsafeCrashWith "Control.Monad.Run: the impossible happened"

unBase ∷ ∀ f. VariantF (base ∷ FProxy f) ~> f
unBase = case_ # on (SProxy ∷ SProxy "base") id

-- | Extracts the value from a program with only base effects. This assumes
-- | stack safety under Monadic recursion.
runBase
  ∷ ∀ f a
  . Monad f
  ⇒ Run (base ∷ FProxy f) a
  → f a
runBase = unwrap >>> loop
  where
  loop = resume' (\b f → f <$> unBase b >>= loop) pure

-- | Extracts the value from a program with only base effects using `MonadRec`
-- | to preserve stack safety.
runBaseRec
  ∷ ∀ f a
  . MonadRec f
  ⇒ Run (base ∷ FProxy f) a
  → f a
runBaseRec = unwrap >>> foldFree (case_ # on (SProxy ∷ SProxy "base") id)

-- | Interprets the base effect into some Monad `m` via continuation passing.
runBaseWithCont
  ∷ ∀ f m a b
  . Monad m
  ⇒ (f (Unit → m b) → m b)
  → (a → m b)
  → Run (base ∷ FProxy f) a
  → m b
runBaseWithCont k1 k2 = unwrap >>> loop
  where
  loop = resume' (\b f → k1 (unBase $ (\b' _ → loop (f b')) <$> b)) k2

-- | Interprets an effect functor into the base effect, eliminating the label.
runWithBase
  ∷ ∀ sym f m r1 r2 a
  . RowCons sym (FProxy f) (base ∷ FProxy m | r2) r1
  ⇒ IsSymbol sym
  ⇒ Functor m
  ⇒ SProxy sym
  → (f (Run r1 a) → m (Run r1 a))
  → Run r1 a
  → Run (base ∷ FProxy m | r2) a
runWithBase p k = runWithEffect p (liftBase <<< k)

-- | Interprets an effect functor in terms of other `Run` effects, eliminating
-- | the label.
runWithEffect
  ∷ ∀ sym f r1 r2 a
  . RowCons sym (FProxy f) r2 r1
  ⇒ IsSymbol sym
  ⇒ SProxy sym
  → (f (Run r1 a) → Run r2 (Run r1 a))
  → Run r1 a
  → Run r2 a
runWithEffect p k = loop
  where
  handle = on p k send
  loop   = unwrap >>> resume' (\b f → handle (RunM <<< f <$> b) >>= loop) pure

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
