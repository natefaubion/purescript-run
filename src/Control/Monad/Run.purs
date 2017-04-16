module Control.Monad.Run
  ( REffect
  , RBase
  , RProxy(..)
  , RunF
  , Run
  , liftRun
  , liftBase
  , peel
  , relay
  , prj
  , run
  , runBase
  , BaseEff
  , BaseAff
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Free (Free, liftF, runFree, foldFree, resume)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol)
import Data.Yoneda (Yoneda, liftYoneda, lowerYoneda)
import Partial.Unsafe (unsafeCrashWith)
import Type.Equality (class TypeEquals)
import Unsafe.Coerce (unsafeCoerce)

data REffect (f ∷ Type → Type)

data RBase (f ∷ Type → Type)

newtype FTag = FTag String

data FBox a

mkFBox ∷ ∀ a f. Yoneda f a → FBox a
mkFBox = unsafeCoerce

runFBox ∷ ∀ a r. (∀ f. Yoneda f a → r) → FBox a → r
runFBox = unsafeCoerce

data RProxy (sym ∷ Symbol) (f ∷ Type → Type) = RProxy

data RunF (r ∷ # Type) a = RunF FTag (FBox a)

newtype Run (r ∷ # Type) a = RunM (Free (RunF r) a)

instance functorRunF ∷ Functor (RunF r) where
  map f (RunF tag ex) = RunF tag $ runFBox (mkFBox <<< map f) ex

derive instance newtypeRun ∷ Newtype (Run r a) _
derive newtype instance functorRun :: Functor (Run r)
derive newtype instance applyRun :: Apply (Run r)
derive newtype instance applicativeRun :: Applicative (Run r)
derive newtype instance bindRun :: Bind (Run r)
derive newtype instance monadRun :: Monad (Run r)

liftRun
  ∷ ∀ sym r1 r2 f a
  . RowCons sym (REffect f) r1 r2
  ⇒ IsSymbol sym
  ⇒ Functor f
  ⇒ RProxy sym f
  → f a
  → Run r2 a
liftRun _ f = RunM $ liftF $ RunF (FTag (reflectSymbol (SProxy ∷ SProxy sym))) (mkFBox (liftYoneda f))

liftBase
  ∷ ∀ r f a
  . Functor f
  ⇒ f a
  → Run (base ∷ RBase f | r) a
liftBase f = RunM $ liftF $ RunF (FTag "base") (mkFBox (liftYoneda f))

peel
  ∷ ∀ a r
  . Run r a
  → Either (RunF r (Run r a)) a
peel (RunM r) = unsafeCoerce (resume r)

relay
  ∷ ∀ sym r1 r2 f a b
  . RowCons sym (REffect f) r1 r2
  ⇒ IsSymbol sym
  ⇒ RProxy sym f
  → (a → Run r1 b)
  → (f a → Run r1 b)
  → RunF r2 a
  → Run r1 b
relay _ loop k r@(RunF (FTag tag) f) =
  if tag == reflectSymbol (SProxy ∷ SProxy sym)
    then k (runFBox (coerceN <<< lowerYoneda) f)
    else RunM (liftF (coerceR r)) >>= loop
  where
  coerceN ∷ ∀ g. g ~> f
  coerceN = unsafeCoerce

  coerceR ∷ RunF r2 a → RunF r1 a
  coerceR = unsafeCoerce

prj
  ∷ ∀ sym r1 r2 f a
  . RowCons sym (REffect f) r1 r2
  ⇒ IsSymbol sym
  ⇒ RProxy sym f
  → RunF r2 a
  → Either (RunF r1 a) (f a)
prj _ r@(RunF (FTag tag) f) =
  if tag == reflectSymbol (SProxy ∷ SProxy sym)
    then Right (runFBox (coerceN <<< lowerYoneda) f)
    else Left (coerceR r)
  where
  coerceN ∷ ∀ g. g ~> f
  coerceN = unsafeCoerce

  coerceR ∷ RunF r2 a → RunF r1 a
  coerceR = unsafeCoerce

run ∷ ∀ a. Run () a → a
run = unwrap >>> runFree \_ → unsafeCrashWith "Control.Monad.Run: the impossible happened"

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

data R (r ∷ # Type)

type BaseEff eff = RBase (Eff eff)

instance monadEffRun ∷ (TypeEquals (R rs) (R (base ∷ RBase m | r)), MonadEff eff m) ⇒ MonadEff eff (Run rs) where
  liftEff = coerceR <<< liftBase' <<< liftEff'
    where
    liftEff' ∷ ∀ a. Eff eff a → m a
    liftEff' = liftEff

    liftBase' ∷ ∀ a. m a → Run (base ∷ RBase m | r) a
    liftBase' = liftBase

    coerceR ∷ Run (base ∷ RBase m | r) ~> Run rs
    coerceR = unsafeCoerce

type BaseAff eff = RBase (Aff eff)

instance monadAffRun ∷ (TypeEquals (R rs) (R (base ∷ RBase m | r)), MonadAff eff m) ⇒ MonadAff eff (Run rs) where
  liftAff = coerceR <<< liftBase' <<< liftAff'
    where
    liftAff' ∷ ∀ a. Aff eff a → m a
    liftAff' = liftAff

    liftBase' ∷ ∀ a. m a → Run (base ∷ RBase m | r) a
    liftBase' = liftBase

    coerceR ∷ Run (base ∷ RBase m | r) ~> Run rs
    coerceR = unsafeCoerce
