module Run
  ( Run(..)
  , lift
  , send
  , extract
  , interpret
  , run
  , interpretRec
  , runRec
  , runCont
  , runAccum
  , runAccumRec
  , runAccumCont
  , peel
  , resume
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
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Class as Eff
import Control.Monad.Free (Free, liftF, runFree, runFreeM, resume')
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Either (Either(..))
import Data.Functor.Variant (FProxy(..), VariantF, case_, default, inj, match, on, onMatch)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..)) as Exports
import Data.Symbol (SProxy(..), class IsSymbol)
import Data.Tuple (Tuple, curry, uncurry)
import Partial.Unsafe (unsafeCrashWith)
import Type.Equality (class TypeEquals)
import Type.Row (RProxy)
import Unsafe.Coerce (unsafeCoerce)

-- | An extensible effect Monad, indexed by a set of effect functors. Effects
-- | are eliminated by interpretation into a pure value or into some base
-- | effect Monad.
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

-- | This instance is provided for compatibility, but is otherwise
-- | unnecessary. You can use monadic recursion with `Run`, deferring the
-- | `MonadRec` constraint till it is interpretted.
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
-- |
-- | ```purescript
-- | type LessRows = (foo :: FOO)
-- | type MoreRows = (foo :: FOO, bar :: BAR, baz :: BAZ)
-- |
-- | foo :: Run LessRows Unit
-- | foo = foo
-- |
-- | foo' :: Run MoreRows Unit
-- | foo' = expand foo
-- | ```
expand
  ∷ ∀ r1 r2 rx a
  . Union r1 rx r2
  ⇒ Run r1 a
  → Run r2 a
expand = unsafeCoerce

-- | Extracts the value from a purely interpreted program.
extract ∷ ∀ a. Run () a → a
extract = unwrap >>> runFree \_ → unsafeCrashWith "Run: the impossible happened"

-- | Extracts the value from a program via some Monad `m`. This assumes
-- | stack safety under Monadic recursion.
interpret
  ∷ ∀ m a r
  . Monad m
  ⇒ (VariantF r ~> m)
  → Run r a
  → m a
interpret = run

-- | Identical to `interpret` but with a less restrictive type signature,
-- | letting you intercept the rest of the program.
run
  ∷ ∀ m a r
  . Monad m
  ⇒ (VariantF r (Run r a) → m (Run r a))
  → Run r a
  → m a
run k = loop
  where
  loop ∷ Run r a → m a
  loop = resume (\a → loop =<< k a) pure

-- | Extracts the value from a program via some MonadRec `m`, preserving
-- | stack safety.
interpretRec
  ∷ ∀ m a r
  . MonadRec m
  ⇒ (VariantF r ~> m)
  → Run r a
  → m a
interpretRec = runRec

-- | Identical to `interpretRec` but with a less restrictive type
-- | signature, letting you intercept the rest of the program.
runRec
  ∷ ∀ m a r
  . MonadRec m
  ⇒ (VariantF r (Run r a) → m (Run r a))
  → Run r a
  → m a
runRec k = runFreeM (coerceM k) <<< unwrap
  where
  -- Just so we can avoid the overhead of mapping the Run constructor
  coerceM ∷ (VariantF r (Run r a) -> m (Run r a)) -> VariantF r (Free (VariantF r) a) -> m (Free (VariantF r) a)
  coerceM = unsafeCoerce

-- | Extracts the value from a program via some `m` using continuation passing.
runCont
  ∷ ∀ m a b r
  . (VariantF r (m b) → m b)
  → (a → m b)
  → Run r a
  → m b
runCont k1 k2 = loop
  where
  loop ∷ Run r a → m b
  loop = resume (\b -> k1 (loop <$> b)) k2

-- | Extracts the value from a program via some Monad `m` with an internal
-- | accumulator. This assumes stack safety under Monadic recursion.
runAccum
  ∷ ∀ m r s a
  . Monad m
  ⇒ (s → VariantF r (Run r a) → m (Tuple s (Run r a)))
  → s
  → Run r a
  → m a
runAccum k = loop
  where
  loop ∷ s → Run r a → m a
  loop s = resume (\b → uncurry loop =<< k s b) pure

-- | Extracts the value from a program via some MonadRec `m` with an internal
-- | accumulator.
runAccumRec
  ∷ ∀ m r s a
  . MonadRec m
  ⇒ (s → VariantF r (Run r a) → m (Tuple s (Run r a)))
  → s
  → Run r a
  → m a
runAccumRec k = curry (tailRecM (uncurry loop))
  where
  loop ∷ s → Run r a → m (Step (Tuple s (Run r a)) a)
  loop s = resume (\b → Loop <$> k s b) (pure <<< Done)

-- | Extracts the value from a program via some `m` using continuation passing
-- | with an internal accumulator.
runAccumCont
  ∷ ∀ m r s a b
  . (s → VariantF r (s → m b) → m b)
  → (a → s → m b)
  → s
  → Run r a
  → m b
runAccumCont k1 k2 = flip loop
  where
  loop ∷ Run r a → s → m b
  loop = resume (\b s → k1 s (loop <$> b)) k2

-- | Type synonym for using `Eff` as an effect.
type EFF eff = FProxy (Eff eff)

-- Lift an `Eff` effect into the `Run` Monad via the `eff` label.
liftEff ∷ ∀ eff r. Eff eff ~> Run (eff ∷ EFF eff | r)
liftEff = lift (SProxy ∷ SProxy "eff")

-- | Runs a base `Eff` effect.
runBaseEff ∷ ∀ eff. Run (eff ∷ EFF eff) ~> Eff eff
runBaseEff = runRec $ match { eff: \a → a }

-- | Type synonym for using `Aff` as an effect.
type AFF eff = FProxy (Aff eff)

-- | Lift an `Aff` effect into the `Run` Monad via the `aff` label.
liftAff ∷ ∀ eff r. Aff eff ~> Run (aff ∷ AFF eff | r)
liftAff = lift (SProxy ∷ SProxy "aff")

-- | Runs a base `Aff` effect.
runBaseAff ∷ ∀ eff. Run (aff ∷ AFF eff) ~> Aff eff
runBaseAff = run $ match { aff: \a → a }

-- | Runs base `Aff` and `Eff` together as one effect.
runBaseAff' ∷ ∀ eff. Run (aff ∷ AFF eff, eff ∷ EFF eff) ~> Aff eff
runBaseAff' = run $ match { aff: \a → a, eff: \a → Eff.liftEff a }

instance runMonadEff ∷ (TypeEquals (RProxy r1) (RProxy (eff ∷ EFF eff | r2))) ⇒ MonadEff eff (Run r1) where
  liftEff = coerceEff <<< liftEff
    where
    coerceEff ∷ Run (eff ∷ EFF eff | r2) ~> Run r1
    coerceEff = unsafeCoerce

-- | This will insert an `EFF` effect because `MonadAff` entails `MonadEff`.
-- | If you don't want this, use `Run.liftAff` rather than `Control.Monad.Aff.Class.liftAff`.
instance runMonadAff ∷ (TypeEquals (RProxy r1) (RProxy (aff ∷ AFF eff, eff ∷ EFF eff | r2))) ⇒ MonadAff eff (Run r1) where
  liftAff = coerceAff <<< liftAff
    where
    coerceAff ∷ Run (aff ∷ AFF eff, eff ∷ EFF eff | r2) ~> Run r1
    coerceAff = unsafeCoerce
