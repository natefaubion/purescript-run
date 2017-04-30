module Test.Streaming where

import Prelude hiding (map)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Run (Run, runBase, liftBase, BaseEff)
import Run.Streaming (YIELD, Producer, Consumer, Transformer, Server, Client, yield, await, request, respond)
import Run.Streaming.Push as Push
import Run.Streaming.Pull as Pull

forever ∷ ∀ r a b. Run r a → Run r b
forever go = go >>= \_ → forever go

map ∷ ∀ x y r a. (x → y) → Transformer r x y a
map f = forever (await >>= f >>> yield)

take ∷ ∀ x r. Int → Transformer r x x Unit
take 0 = pure unit
take n = do
  await >>= yield
  take (n - 1)

naturals ∷ ∀ r a. Producer r Int a
naturals = go 1
  where
  go n = do
    yield n
    go (n + 1)

toConsole ∷ ∀ eff r a. Consumer (base ∷ BaseEff (console ∷ CONSOLE | eff) | r) String a
toConsole = forever (await >>= log >>> liftBase)

data Req
  = A Int
  | B Int

type Rep
  = String

client ∷ ∀ r a. Client (yield ∷ YIELD String | r) Req String a
client = Push.for naturals \n → do
  s ← request if (n / 2 * 2) /= n then A n else B n
  yield s

server ∷ ∀ r a. Int → Req → Server r Req Rep a
server m req =
  respond case req of
    A n → "A: " <> show n <> " @ " <> show m
    B n → "B: " <> show n <> " @ " <> show m
  >>= server (m + 10)

main ∷ Eff (console ∷ CONSOLE) Unit
main = do
  runBase $
    naturals
      `Push.fuse` take 10
      `Push.fuse` map (show >>> append "Push: ")
      `Push.fuse` toConsole

  runBase $
    toConsole
      `Pull.fuse` map (append "Pull: " <<< show)
      `Pull.fuse` take 10
      `Pull.fuse` naturals

  runBase $
    Push.for naturals (show >>> yield)
      `Push.fuse` take 10
      `Push.fuse` toConsole

  runBase $
    toConsole
      `Pull.fuse` take 10
      `Pull.fuse` (client `Pull.chain` server 10)
