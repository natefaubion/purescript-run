module Control.Monad.Run
  ( Run
  , RunF
  , RProxy(..)
  , REffect
  , RBase
  , run
  , runBase
  , interpret
  , liftEffect
  , liftBase
  , peel
  , send
  , decomp
  , BaseEff
  , BaseAff
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Free (Free, liftF, runFree, foldFree, substFree, resume)
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap, wrap, over)
import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol)
import Data.Yoneda (Yoneda, liftYoneda, lowerYoneda, hoistYoneda)
import Partial.Unsafe (unsafeCrashWith)
import Type.Equality (class TypeEquals)
import Unsafe.Coerce (unsafeCoerce)

-- | Phantom type for effect rows. Effect functors need to be tagged because
-- | the type level row constraints only works on rows of kind `Type`.
data REffect (f ∷ Type → Type)

-- | Phantom type the base, or terminal, effect. This may be something like
-- | `Eff`, `Aff`, or `IO`.
data RBase (f ∷ Type → Type)

newtype FTag = FTag String

data FBox a

mkFBox ∷ ∀ a f. Yoneda f a → FBox a
mkFBox = unsafeCoerce

runFBox ∷ ∀ a r. (∀ f. Yoneda f a → r) → FBox a → r
runFBox = unsafeCoerce

-- | A proxy type which links a label with an effect functor.
data RProxy (sym ∷ Symbol) (f ∷ Type → Type) = RProxy

-- | An opaque instruction in the `Run` Monad. Instructions can be inspected
-- | using `decomp` and an `RProxy` for a given effect.
data RunF (r ∷ # Type) a = RunF FTag (FBox a)

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
newtype Run (r ∷ # Type) a = RunM (Free (RunF r) a)

instance functorRunF ∷ Functor (RunF r) where
  map f (RunF tag ex) = RunF tag $ runFBox (mkFBox <<< map f) ex

derive instance newtypeRun ∷ Newtype (Run r a) _
derive newtype instance functorRun :: Functor (Run r)
derive newtype instance applyRun :: Apply (Run r)
derive newtype instance applicativeRun :: Applicative (Run r)
derive newtype instance bindRun :: Bind (Run r)
derive newtype instance monadRun :: Monad (Run r)

-- | Lifts an effect functor into the `Run` Monad according to the provided
-- | `RProxy` slot.
liftEffect
  ∷ ∀ sym r1 r2 f a
  . RowCons sym (REffect f) r1 r2
  ⇒ IsSymbol sym
  ⇒ Functor f
  ⇒ RProxy sym f
  → f a
  → Run r2 a
liftEffect _ f = RunM $ liftF $ RunF (FTag (reflectSymbol (SProxy ∷ SProxy sym))) (mkFBox (liftYoneda f))

-- | Lifts a base effect into the `Run` Monad (eg. `Eff`, `Aff`, or `IO`).
liftBase
  ∷ ∀ r f a
  . Functor f
  ⇒ f a
  → Run (base ∷ RBase f | r) a
liftBase f = RunM $ liftF $ RunF (FTag "base") (mkFBox (liftYoneda f))

-- | Reflects the next instruction or the final value if there are no more
-- | instructions.
peel
  ∷ ∀ a r
  . Run r a
  → Either (RunF r (Run r a)) a
peel (RunM r) = unsafeCoerce (resume r)

-- | Enqueues an instruction in the `Run` Monad.
send
  ∷ ∀ a r
  . RunF r a
  → Run r a
send = wrap <<< liftF

-- | Attempts to read an instruction according to the provided `RProxy`. If the
-- | instruction does not match, it will yield the instruction with the effect
-- | label removed.
decomp
  ∷ ∀ sym r1 r2 f a
  . RowCons sym (REffect f) r1 r2
  ⇒ IsSymbol sym
  ⇒ RProxy sym f
  → RunF r2 a
  → Either (RunF r1 a) (f a)
decomp _ r@(RunF (FTag tag) f) =
  if tag == reflectSymbol (SProxy ∷ SProxy sym)
    then Right (runFBox (coerceN <<< lowerYoneda) f)
    else Left (coerceR r)
  where
  coerceN ∷ ∀ g. g ~> f
  coerceN = unsafeCoerce

  coerceR ∷ RunF r2 a → RunF r1 a
  coerceR = unsafeCoerce

-- | Extracts the value from a fully interpreted program.
run ∷ ∀ a. Run () a → a
run = unwrap >>> runFree \_ → unsafeCrashWith "Control.Monad.Run: the impossible happened"

-- | Extracts the value from a program with only base effects.
runBase
  ∷ ∀ f a
  . MonadRec f
  ⇒ Run (base ∷ RBase f) a
  → f a
runBase = unwrap >>> foldFree go
  where
  go ∷ RunF (base ∷ RBase f) ~> f
  go (RunF _ fb) = runFBox (coerceY <<< lowerYoneda) fb

  coerceY ∷ ∀ g. g ~> f
  coerceY = unsafeCoerce

-- | Interprets an effect functor into the base effect, eliminating the label.
interpret
  ∷ ∀ sym f m r1 r2 r3 a
  . RowCons sym (REffect f) r2 r1
  ⇒ RowCons "base" (RBase m) r2 r3
  ⇒ IsSymbol sym
  ⇒ RProxy sym f
  → (f ~> m)
  → Run r1 a
  → Run r3 a
interpret _ k = over RunM (substFree (\a → liftF (go a)))
  where
  tag =
    reflectSymbol (SProxy ∷ SProxy sym)

  go ∷ RunF r1 ~> RunF r3
  go r@(RunF (FTag tag') f) =
    if tag == tag'
      then RunF (FTag "base") (runFBox (mkFBox <<< hoistYoneda (k <<< coerceN)) f)
      else coerceR r

  coerceN ∷ ∀ g. g ~> f
  coerceN = unsafeCoerce

  coerceR ∷ RunF r1 ~> RunF r3
  coerceR = unsafeCoerce

data R (r ∷ # Type)

-- Type synonym for using `Eff` as a base effect.
type BaseEff eff = RBase (Eff eff)

-- Type synonym for using `Aff` as a base effect.
type BaseAff eff = RBase (Aff eff)

instance monadRecRun ∷ MonadRec (Run r) where
  tailRecM f = loop
    where
    loop a = do
      b ← f a
      case b of
        Done r → pure r
        Loop n → loop n

instance monadEffRun ∷ (TypeEquals (R rs) (R (base ∷ RBase m | r)), MonadEff eff m) ⇒ MonadEff eff (Run rs) where
  liftEff = coerceR <<< liftBase <<< liftEff
    where
    coerceR ∷ Run (base ∷ RBase m | r) ~> Run rs
    coerceR = unsafeCoerce

instance monadAffRun ∷ (TypeEquals (R rs) (R (base ∷ RBase m | r)), MonadAff eff m) ⇒ MonadAff eff (Run rs) where
  liftAff = coerceR <<< liftBase <<< liftAff
    where
    coerceR ∷ Run (base ∷ RBase m | r) ~> Run rs
    coerceR = unsafeCoerce
