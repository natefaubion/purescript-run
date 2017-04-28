module Control.Monad.Run.Streaming
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
  , runStep
  , runConsumer
  , runProducer
  , fuse
  , push
  , (!>)
  , pull
  , (<!)
  ) where

import Prelude
import Control.Monad.Run (Run, SProxy(..), FProxy)
import Control.Monad.Run as Run
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)

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

type Producer r a o = Resume r a Unit o

type Consumer r a i = Resume r a i Unit

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

runProducer
  ∷ ∀ r a o
  . Run (yield ∷ YIELD o | r) a
  → Run r (Producer r a o)
runProducer = runStep _yield

runConsumer
  ∷ ∀ r a i
  . Run (await ∷ AWAIT i | r) a
  → Run r (Consumer r a i)
runConsumer = runStep _await

fuse
  ∷ ∀ i o r a
  . Resume r a i o
  → Resume r a o i
  → Run r a
fuse = case _, _ of
  Next o k, Next i j → join $ fuse <$> k i <*> j o
  Done x, _ → pure x
  _, Done x → pure x

push
  ∷ ∀ x r a
  . Run (yield ∷ YIELD x | r) a
  → Run (await ∷ AWAIT x | r) a
  → Run r a
push ra rb = join $ fuse <$> runProducer ra <*> runConsumer rb

infixl 6 push as !>

pull
  ∷ ∀ x r a
  . Run (await ∷ AWAIT x | r) a
  → Run (yield ∷ YIELD x | r) a
  → Run r a
pull ra rb = join $ fuse <$> runConsumer ra <*> runProducer rb

infixl 6 pull as <!
