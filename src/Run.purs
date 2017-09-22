module Run
  ( Run(..)
  , run
  , runBase
  , runBaseWithCont
  , runWithBase
  , runWithEffect
  , interpretWithBase
  , interpretWithEffect
  , foldWithEffect
  , liftEffect
  , liftBase
  , peel
  , resume
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
import Data.Tuple (Tuple, uncurry)
import Partial.Unsafe (unsafeCrashWith)
import Type.Equality (class TypeEquals)
import Unsafe.Coerce (unsafeCoerce)

-- | An extensible effect Monad, indexed by a set of effect functors. Effects
-- | are eliminated by interpretation into a pure value or into some base
-- | effect Monad. The `Run` Monad is an alternative to Monad Transformers.
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
newtype Run (r ∷ # Type) a = Run (Free (VariantF r) a)

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
liftEffect p = Run <<< liftF <<< inj p

-- | Lifts a base effect into the `Run` Monad (eg. `Eff`, `Aff`, or `IO`).
liftBase
  ∷ ∀ r f a
  . Functor f
  ⇒ f a
  → Run (base ∷ FProxy f | r) a
liftBase = Run <<< liftF <<< inj (SProxy ∷ SProxy "base")

-- | Reflects the next instruction or the final value if there are no more
-- | instructions.
peel
  ∷ ∀ a r
  . Run r a
  → Either (VariantF r (Run r a)) a
peel = resume Left Right

-- | Eliminator for the `Run` data type.
resume
  ∷ ∀ a b r
  . (VariantF r (Run r a) → b)
  → (a → b)
  → Run r a
  → b
resume k1 k2 = resume' (\x f → k1 (Run <<< f <$> x)) k2 <<< unwrap

-- | Enqueues an instruction in the `Run` Monad.
send
  ∷ ∀ a r
  . VariantF r a
  → Run r a
send = Run <<< liftF

-- | Casts some set of effects to a wider set of effects via a left-biased
-- | union. For example, you could take a closed effect and unify it with
-- | a superset of effects because we know the additional effects never
-- | occur.
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
runBase = loop
  where
  loop ∷ Run (base ∷ FProxy f) a → f a
  loop = resume (\b → loop =<< unBase b) pure

-- | Extracts the value from a program with only base effects using `MonadRec`
-- | to preserve stack safety.
runBaseRec
  ∷ ∀ f a
  . MonadRec f
  ⇒ Run (base ∷ FProxy f) a
  → f a
runBaseRec = foldFree unBase <<< unwrap

-- | Interprets the base effect into some Monad `m` via continuation passing.
runBaseWithCont
  ∷ ∀ f m a b
  . Monad m
  ⇒ (f (m b) → m b)
  → (a → m b)
  → Run (base ∷ FProxy f) a
  → m b
runBaseWithCont k1 k2 = loop
  where
  loop ∷ Run (base ∷ FProxy f) a → m b
  loop = resume (\b -> k1 (unBase (loop <$> b))) k2

-- | Interprets an effect functor into the base effect via a natural
-- | transformation, eliminating the label.
interpretWithBase
  ∷ ∀ sym f m r1 r2 a
  . RowCons sym (FProxy f) (base ∷ FProxy m | r2) r1
  ⇒ IsSymbol sym
  ⇒ Functor m
  ⇒ SProxy sym
  → (f ~> m)
  → Run r1 a
  → Run (base ∷ FProxy m | r2) a
interpretWithBase = runWithBase

-- | The same as `interpretWithBase`, but with a less restrictive type for the
-- | interpreter.
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
interpretWithEffect
  ∷ ∀ sym f r1 r2 a
  . RowCons sym (FProxy f) r2 r1
  ⇒ IsSymbol sym
  ⇒ SProxy sym
  → (f ~> Run r2)
  → Run r1 a
  → Run r2 a
interpretWithEffect = runWithEffect

-- | The same as `interpretWithEffect` but with a less restrictive type for the
-- | interpreter.
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
  handle ∷ VariantF r1 (Run r1 a) → Run r2 (Run r1 a)
  handle = on p k send

  loop ∷ Run r1 a → Run r2 a
  loop = resume (\b → handle b >>= loop) pure

-- | Interprets an effect in terms of other `Run` effects with an internal
-- | accumulator.
foldWithEffect
  ∷ ∀ sym f r1 r2 s a b
  . RowCons sym (FProxy f) r2 r1
  ⇒ IsSymbol sym
  ⇒ SProxy sym
  → (s → f (Run r1 a) → Run r2 (Tuple s (Run r1 a)))
  → Run r2 s
  → Run r1 a
  → Run r2 a
foldWithEffect p k init r = init >>= flip loop r
  where
  handle ∷ s → VariantF r1 (Run r1 a) → Run r2 a
  handle acc =
    on p
      (k acc >=> uncurry loop)
      (send >=> loop acc)

  loop ∷ s → Run r1 a → Run r2 a
  loop acc = resume (handle acc) pure

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
