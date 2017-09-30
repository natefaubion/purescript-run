module Run
  ( Run(..)
  , run
  , runEffect
  , runEffectRec
  , runEffectCont
  , runWithEffect
  , interpretWithEffect
  , foldWithEffect
  , lift
  , peel
  , resume
  , send
  , expand
  , EFF
  , AFF
  , liftEff
  , liftAff
  , runBaseEff
  , runBaseAff
  , runBaseAff'
  , module Data.Functor.Variant
  , module Exports
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class as Eff
import Control.Monad.Free (Free, liftF, runFree, runFreeM, resume')
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Data.Either (Either(..))
import Data.Functor.Variant (FProxy(..), VariantF, case_, default, inj, match, on, onMatch)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..)) as Exports
import Data.Symbol (SProxy(..), class IsSymbol)
import Data.Tuple (Tuple, uncurry)
import Partial.Unsafe (unsafeCrashWith)
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
-- |   , eff ∷ EFF (console ∷ CONSOLE)
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
-- |     # runBaseEff
-- |     # void
-- | ````
newtype Run (r ∷ # Type) a = Run (Free (VariantF r) a)

derive instance newtypeRun ∷ Newtype (Run r a) _
derive newtype instance functorRun :: Functor (Run r)
derive newtype instance applyRun :: Apply (Run r)
derive newtype instance applicativeRun :: Applicative (Run r)
derive newtype instance bindRun :: Bind (Run r)
derive newtype instance monadRun :: Monad (Run r)

instance monadRecRun ∷ MonadRec (Run r) where
  tailRecM f = loop
    where
    loop a = do
      b ← f a
      case b of
        Done r → pure r
        Loop n → loop n

-- | Lifts an effect functor into the `Run` Monad according to the provided
-- | `SProxy` slot.
lift
  ∷ ∀ sym r1 r2 f a
  . RowCons sym (FProxy f) r1 r2
  ⇒ IsSymbol sym
  ⇒ Functor f
  ⇒ SProxy sym
  → f a
  → Run r2 a
lift p = Run <<< liftF <<< inj p

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

-- | Extracts the value from a program with only base effects. This assumes
-- | stack safety under Monadic recursion.
runEffect
  ∷ ∀ m a r
  . Monad m
  ⇒ (VariantF r (Run r a) → m (Run r a))
  → Run r a
  → m a
runEffect k = loop
  where
  loop ∷ Run r a → m a
  loop = resume (\a → loop =<< k a) pure

-- | Extracts the value from a program with only base effects using `MonadRec`
-- | to preserve stack safety.
runEffectRec
  ∷ ∀ m a r
  . MonadRec m
  ⇒ (VariantF r (Run r a) → m (Run r a))
  → Run r a
  → m a
runEffectRec k = runFreeM (coerceM k) <<< unwrap
  where
  -- Just so we can avoid the overhead of mapping the Run constructor
  coerceM ∷ (VariantF r (Run r a) -> m (Run r a)) -> VariantF r (Free (VariantF r) a) -> m (Free (VariantF r) a)
  coerceM = unsafeCoerce

-- | Interprets the base effect into some Monad `m` via continuation passing.
runEffectCont
  ∷ ∀ m a b r
  . Monad m
  ⇒ (VariantF r (m b) → m b)
  → (a → m b)
  → Run r a
  → m b
runEffectCont k1 k2 = loop
  where
  loop ∷ Run r a → m b
  loop = resume (\b -> k1 (loop <$> b)) k2

-- | Interprets an effect functor in terms of other `Run` effects, eliminating
-- | the label.
interpretWithEffect
  ∷ ∀ r1 r2 a
  . (VariantF r1 ~> Run r2)
  → Run r1 a
  → Run r2 a
interpretWithEffect = runWithEffect

-- | The same as `interpretWithEffect` but with a less restrictive type for the
-- | interpreter.
runWithEffect
  ∷ ∀ r1 r2 a
  . (VariantF r1 (Run r1 a) → Run r2 (Run r1 a))
  → Run r1 a
  → Run r2 a
runWithEffect k = loop
  where
  loop ∷ Run r1 a → Run r2 a
  loop = resume (\b → loop =<< k b) pure

-- | Interprets an effect in terms of other `Run` effects with an internal
-- | accumulator.
foldWithEffect
  ∷ ∀ r1 r2 s a
  . (VariantF r1 (Run r1 a) → s → Run r2 (Tuple (Run r1 a) s))
  → (Run r2 s)
  → Run r1 a
  → Run r2 a
foldWithEffect step init r = init >>= loop r
  where
  loop ∷ Run r1 a → s → Run r2 a
  loop = resume (\b → uncurry loop <=< step b) (const <<< pure)

-- | Type synonym for using `Eff` as an effect.
type EFF eff = FProxy (Eff eff)

-- Lift an `Eff` effect into the `Run` Monad via the `eff` label.
liftEff ∷ ∀ eff r. Eff eff ~> Run (eff ∷ EFF eff | r)
liftEff = lift (SProxy ∷ SProxy "eff")

-- | Runs a base `Eff` effect.
runBaseEff ∷ ∀ eff. Run (eff ∷ EFF eff) ~> Eff eff
runBaseEff = runEffectRec $ match { eff: \a → a }

-- | Type synonym for using `Aff` as an effect.
type AFF eff = FProxy (Aff eff)

-- | Lift an `Aff` effect into the `Run` Monad via the `aff` label.
liftAff ∷ ∀ eff r. Aff eff ~> Run (aff ∷ AFF eff | r)
liftAff = lift (SProxy ∷ SProxy "aff")

-- | Runs a base `Aff` effect.
runBaseAff ∷ ∀ eff. Run (aff ∷ AFF eff) ~> Aff eff
runBaseAff = runEffect $ match { aff: \a → a }

-- | Runs base `Aff` and `Eff` together as one effect.
runBaseAff' ∷ ∀ eff. Run (aff ∷ AFF eff, eff ∷ EFF eff) ~> Aff eff
runBaseAff' = runEffect $ match { aff: \a → a, eff: \a → Eff.liftEff a }
