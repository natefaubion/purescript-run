module Run
  ( Run(..)
  , lift
  , send
  , extract
  , interpret
  , interpretRec
  , run
  , runRec
  , runCont
  , runPure
  , runAccum
  , runAccumRec
  , runAccumCont
  , runAccumPure
  , peel
  , resume
  , expand
  , EFFECT
  , AFF
  , liftEffect
  , liftAff
  , runBaseEffect
  , runBaseAff
  , runBaseAff'
  , module Data.Functor.Variant
  , module Exports
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class as Effect
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Control.Monad.Free (Free, liftF, runFree, runFreeM, resume')
import Control.Monad.Rec.Class (Step(..)) as Exports
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Plus (class Plus)
import Data.Either (Either(..))
import Data.Functor.Variant (FProxy(..), VariantF, case_, default, inj, match, on, onMatch)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..)) as Exports
import Data.Symbol (SProxy(..), class IsSymbol)
import Data.Tuple (Tuple(..), curry, uncurry)
import Partial.Unsafe (unsafeCrashWith)
import Run.Internal (_choose, CHOOSE, Choose(..), _empty, EMPTY, Empty(..), toRows, fromRows)
import Type.Equality (class TypeEquals)
import Prim.Row as Row
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
-- |   , effect ∷ EFFECT
-- |   )
-- |
-- | yesProgram ∷ Run MyEffects Unit
-- | yesProgram = do
-- |   whenM (gets (_ < 0)) do
-- |     throw "Number is less than 0"
-- |   whileM_ (gets (_ > 0)) do
-- |     liftEffect $ log "Yes"
-- |     modify (_ - 1)
-- |   where
-- |   whileM_
-- |     ∷ ∀ a
-- |     . Run MyEffects Boolean
-- |     → Run MyEffects a
-- |     → Run MyEffects Unit
-- |   whileM_ mb ma = flip tailRecM unit \a →
-- |     mb >>= if _ then ma $> Loop unit else pure $ Done unit
-- |
-- | main =
-- |   yesProgram
-- |     # catch (liftEffect <<< log)
-- |     # runState 10
-- |     # runBaseEffect
-- |     # void
-- | ````
newtype Run r a = Run (Free (VariantF r) a)

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
  . Row.Cons sym (FProxy f) r1 r2
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
  . Row.Union r1 rx r2
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
  → (s → a → m b)
  → s
  → Run r a
  → m b
runAccumCont k1 k2 = loop
  where
  loop ∷ s → Run r a → m b
  loop s = resume (\b → k1 s (flip loop <$> b)) (k2 s)

-- | Eliminates effects purely. Uses `Step` from `Control.Monad.Rec.Class` to
-- | preserve stack safety under tail recursion.
runPure
  ∷ ∀ r1 r2 a
  . (VariantF r1 (Run r1 a) → Step (Run r1 a) (VariantF r2 (Run r1 a)))
  → Run r1 a
  → Run r2 a
runPure k = loop
  where
  loop ∷ Run r1 a → Run r2 a
  loop r = case peel r of
    Left a → case k a of
      Loop r' → loop r'
      Done a' → send a' >>= runPure k
    Right a →
      pure a

-- | Eliminates effects purely with an internal accumulator. Uses `Step` from
-- | `Control.Monad.Rec.Class` to preserve stack safety under tail recursion.
runAccumPure
  ∷ ∀ r1 r2 a b s
  . (s → VariantF r1 (Run r1 a) → Step (Tuple s (Run r1 a)) (VariantF r2 (Run r1 a)))
  → (s → a → b)
  → s
  → Run r1 a
  → Run r2 b
runAccumPure k1 k2 = loop
  where
  loop ∷ s → Run r1 a → Run r2 b
  loop s r = case peel r of
    Left a → case k1 s a of
      Loop (Tuple s' r') → loop s' r'
      Done a' → send a' >>= runAccumPure k1 k2 s
    Right a →
      pure (k2 s a)

-- | Type synonym for using `Effect` as an effect.
type EFFECT = FProxy Effect

-- Lift an `Effect` effect into the `Run` Monad via the `effect` label.
liftEffect ∷ ∀ r. Effect ~> Run (effect ∷ EFFECT | r)
liftEffect = lift (SProxy ∷ SProxy "effect")

-- | Runs a base `Effect` effect.
runBaseEffect ∷ Run (effect ∷ EFFECT) ~> Effect
runBaseEffect = runRec $ match { effect: \a → a }

-- | Type synonym for using `Aff` as an effect.
type AFF = FProxy Aff

-- | Lift an `Aff` effect into the `Run` Monad via the `aff` label.
liftAff ∷ ∀ r. Aff ~> Run (aff ∷ AFF | r)
liftAff = lift (SProxy ∷ SProxy "aff")

-- | Runs a base `Aff` effect.
runBaseAff ∷ Run (aff ∷ AFF) ~> Aff
runBaseAff = run $ match { aff: \a → a }

-- | Runs base `Aff` and `Effect` together as one effect.
runBaseAff' ∷ Run (aff ∷ AFF, effect ∷ EFFECT) ~> Aff
runBaseAff' = run $ match { aff: \a → a, effect: \a → Effect.liftEffect a }

instance runMonadEffect ∷ (TypeEquals (RProxy r1) (RProxy (effect ∷ EFFECT | r2))) ⇒ MonadEffect (Run r1) where
  liftEffect = fromRows <<< liftEffect

-- | This will insert an `EFFECT` effect because `MonadAff` entails `MonadEffect`.
-- | If you don't want this, use `Run.liftAff` rather than `Control.Monad.Aff.Class.liftAff`.
instance runMonadAff ∷ (TypeEquals (RProxy r1) (RProxy (aff ∷ AFF, effect ∷ EFFECT | r2))) ⇒ MonadAff (Run r1) where
  liftAff = fromRows <<< liftAff

liftChoose ∷ ∀ r a. Choose a → Run (choose ∷ CHOOSE | r) a
liftChoose = lift _choose

liftEmpty :: forall r a. Empty a -> Run (empty :: EMPTY | r) a
liftEmpty = lift _empty

instance runAlt ∷ (TypeEquals (RProxy r1) (RProxy (choose ∷ CHOOSE | r2))) ⇒ Alt (Run r1) where
  alt a b = fromRows $ liftChoose (Alt identity) >>= if _ then toRows a else toRows b

instance runPlus ∷ (TypeEquals (RProxy r1) (RProxy (choose :: CHOOSE, empty ∷ EMPTY | r2))) ⇒ Plus (Run r1) where
  empty = fromRows $ liftEmpty Empty

instance runAlternative ∷ (TypeEquals (RProxy r1) (RProxy (choose ∷ CHOOSE, empty :: EMPTY | r2))) ⇒ Alternative (Run r1)
