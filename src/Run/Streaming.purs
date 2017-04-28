-- | This module defines primitive combinators for both push and pull streams.
-- |
-- | ```purescript
-- | map ∷ ∀ x y r a. (x → y) → Transformer r x y a
-- | map f = forever do
-- |   x ← await
-- |   yield (f x)
-- |
-- | take ∷ ∀ x r. Int → Transformer r x x Unit
-- | take n
-- |   | n <= 0    = pure unit
-- |   | otherwise = do
-- |       await >>= yield
-- |       take (n - 1)
-- |
-- | naturals ∷ ∀ r a. Producer r Int a
-- | naturals = go 1
-- |   where
-- |   go n = do
-- |     yield n
-- |     go (n + 1)
-- |
-- | toConsole ∷ ∀ eff r a. Consumer (base ∷ BaseEff (console ∷ CONSOLE | eff) | r) String a
-- | toConsole = forever (await >>= log >>> liftBase)
-- |
-- | main ∷ Eff (console ∷ CONSOLE) Unit
-- | main = runBase $
-- |   naturals
-- |   !> take 100
-- |   !> map show
-- |   !> toConsole
-- | ```

module Run.Streaming
  ( Step(..)
  , YIELD
  , AWAIT
  , _yield
  , _await
  , yield
  , await
  , Resume(..)
  , Producer
  , Consumer
  , Transformer
  , runStep
  , runConsumer
  , runProducer
  , fuse
  , push
  , (!>)
  , pull
  , (!<)
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Profunctor (class Profunctor, dimap)
import Data.Symbol (class IsSymbol)
import Run (Run, SProxy(..), FProxy)
import Run as Run

data Step i o a = Step o (i → a)

derive instance functorStep ∷ Functor (Step i o)

type YIELD a = FProxy (Step Unit a)

_yield ∷ SProxy "yield"
_yield = SProxy

liftYield ∷ ∀ o r.  Step Unit o ~> Run (yield ∷ YIELD o | r)
liftYield = Run.liftEffect _yield

yield ∷ ∀ o r. o → Run (yield ∷ YIELD o | r) Unit
yield o = liftYield $ Step o id

type AWAIT a = FProxy (Step a Unit)

_await ∷ SProxy "await"
_await = SProxy

liftAwait ∷ ∀ i r. Step i Unit ~> Run (await ∷ AWAIT i | r)
liftAwait = Run.liftEffect _await

await ∷ ∀ i r. Run (await ∷ AWAIT i | r) i
await = liftAwait $ Step unit id

data Resume r a i o
  = Next o (i → Run r (Resume r a i o))
  | Done a

instance functorResume ∷ Functor (Resume r a i) where
  map f = case _ of
    Next o k → Next (f o) (map (map f) <$> k)
    Done a   → Done a

instance profunctorResume ∷ Profunctor (Resume r a) where
  dimap f g = case _ of
    Next o k → Next (g o) (dimap f (map (dimap f g)) k)
    Done a   → Done a

type Producer r o a = Run (yield ∷ YIELD o | r) a

type Consumer r i a = Run (await ∷ AWAIT i | r) a

type Transformer r i o a = Run (await ∷ AWAIT i, yield ∷ YIELD o | r) a

runStep
  ∷ ∀ sym i o r1 r2 a
  . RowCons sym (FProxy (Step i o)) r1 r2
  ⇒ IsSymbol sym
  ⇒ SProxy sym
  → Run r2 a
  → Run r1 (Resume r1 a i o)
runStep p = loop
  where
  handle = Run.on p Left Right
  loop r = case Run.peel r of
    Left a → case handle a of
      Left (Step o k) →
        pure (Next o (k >>> loop))
      Right a' →
        Run.send a' >>= loop
    Right a →
      pure (Done a)

runProducer ∷ ∀ r a o. Producer r o a → Run r (Resume r a Unit o)
runProducer = runStep _yield

runConsumer ∷ ∀ r a i. Consumer r i a → Run r (Resume r a i Unit)
runConsumer = runStep _await

fuse ∷ ∀ i o r a. Resume r a i o → Resume r a o i → Run r a
fuse = case _, _ of
  Next o k, Next i j → join $ fuse <$> k i <*> j o
  Done x, _ → pure x
  _, Done x → pure x

push ∷ ∀ r a o. Producer r o a → Consumer r o a → Run r a
push ra rb = join $ fuse <$> runProducer ra <*> runConsumer rb

infixl 6 push as !>

pull ∷ ∀ r a o. Consumer r o a → Producer r o a → Run r a
pull ra rb = join $ fuse <$> runConsumer ra <*> runProducer rb

infixr 6 pull as !<
