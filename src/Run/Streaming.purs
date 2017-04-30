-- | This module defines primitives for bidirectional streams analagous to
-- | the Haskell `Pipes` library. Namely, streams may be either pull or pull
-- | and can propagate information both upstream and downstream.

module Run.Streaming
  ( Step(..)
  , STEP
  , YIELD
  , AWAIT
  , REQUEST
  , RESPOND
  , _yield
  , _await
  , yield
  , await
  , request
  , respond
  , Resume(..)
  , Producer
  , Consumer
  , Transformer
  , Client
  , Server
  , Pipe
  , runStep
  , runYield
  , runAwait
  , fuse
  , interleave
  , substitute
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Profunctor (class Profunctor, dimap)
import Data.Symbol (class IsSymbol)
import Run (Run, SProxy(..), FProxy)
import Run as Run

data Step i o a = Step o (i → a)

derive instance functorStep ∷ Functor (Step i o)

type STEP i o = FProxy (Step i o)

type YIELD a = STEP Unit a

type AWAIT a = STEP a Unit

type REQUEST req res = STEP res req

type RESPOND req res = STEP req res

_yield ∷ SProxy "yield"
_yield = SProxy

_await ∷ SProxy "await"
_await = SProxy

liftYield ∷ ∀ req res r.  Step res req ~> Run (yield ∷ STEP res req | r)
liftYield = Run.liftEffect _yield

liftAwait ∷ ∀ req res r. Step req res ~> Run (await ∷ STEP req res | r)
liftAwait = Run.liftEffect _await

-- | Yields a response and waits for a request.
respond ∷ ∀ req res r. res → Run (yield ∷ RESPOND req res | r) req
respond res = liftYield (Step res id)

-- | Issues a request and awaits a response.
request ∷ ∀ req res r. req → Run (await ∷ REQUEST req res | r) res
request req = liftAwait (Step req id)

-- | Yields a value to be consumed downstream.
yield ∷ ∀ o r. o → Run (yield ∷ YIELD o | r) Unit
yield = respond

-- | Awaits a value upstream.
await ∷ ∀ i r. Run (await ∷ AWAIT i | r) i
await = request unit

-- | Producers yield values of type `o` using effects `r`.
type Producer r o = Run (yield ∷ YIELD o | r)

-- | Consumers await values of type `i` using effects `r`.
type Consumer r i = Run (await ∷ AWAIT i | r)

-- | Transformers await values `i` and yield values `o` using effects `r`.
type Transformer r i o = Run (await ∷ AWAIT i, yield ∷ YIELD o | r)

-- | Servers reply to requests `req` with responses `res` using effects `r`.
type Server r req res = Run (yield ∷ RESPOND req res | r)

-- | Clients issue requests `req` and await responses `res` using effects `r`.
type Client r req res = Run (await ∷ REQUEST req res | r)

-- | A full bidirectional Pipe acts as an upstream Client and a downstream Server.
type Pipe r req res req' res' = Run (await ∷ REQUEST req res, yield ∷ RESPOND req' res' | r)

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

runYield ∷ ∀ r a i o. Server r i o a → Run r (Resume r a i o)
runYield = runStep _yield

runAwait ∷ ∀ r a i o. Client r i o a → Run r (Resume r a o i)
runAwait = runStep _await

-- | Pairs inputs and outputs with a left bias.
fuse ∷ ∀ r a i o. Resume r a i o → Resume r a o i → Run r a
fuse = case _, _ of
  Next o k, Next i j → join $ fuse <$> k i <*> j o
  Done x, _ → pure x
  _, Done x → pure x

-- | Subsitutes the outputs of the second argument with the continuation of the
-- | first argument, and vice versa, interleaving the two.
interleave ∷ ∀ r a i o. (o → Run r (Resume r a o i)) → Resume r a i o → Run r a
interleave k = case _ of
  Next o next → k o >>= interleave next
  Done a → pure a

-- | Substitutes the outputs of the second argument with the effects of the
-- | first argument, feeding the result back in to the stream.
substitute ∷ ∀ r a i o. (o → Run r i) → Resume r a i o → Run r a
substitute k = go
  where
  go = case _ of
    Next o next → k o >>= next >>= go
    Done a → pure a
