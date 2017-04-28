module Test.Streaming where

import Prelude hiding (map)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Run (Run, runBase, withEffect, liftBase, FProxy(..), BaseEff)
import Control.Monad.Run.Streaming (YIELD, AWAIT, (!>), yield, _yield, await, _await)

forever ∷ ∀ r a b. Run r a → Run r b
forever go = go >>= \_ → forever go

for
  ∷ ∀ x y r a
  . (x → Run r y)
  → Run (await ∷ AWAIT x, yield ∷ YIELD y | r) a
for f = forever do
  x ← await
  y ← coerceR (f x)
  yield y
  where
  coerceR = withEffect _await (FProxy ∷ AWAIT x)
        >>> withEffect _yield (FProxy ∷ YIELD y)

map
  ∷ ∀ x y r a
  . (x → y)
  → Run (await ∷ AWAIT x, yield ∷ YIELD y | r) a
map f = forever (await >>= (f >>> yield))

take
  ∷ ∀ x r
  . Int
  → Run (await ∷ AWAIT x, yield ∷ YIELD x | r) Unit
take n
  | n <= 0    = pure unit
  | otherwise = do
      await >>= yield
      take (n - 1)

naturals ∷ ∀ r a. Run (yield ∷ YIELD Int | r) a
naturals = go 1
  where
  go n = do
    yield n
    go (n + 1)

toConsole ∷ ∀ eff r a. Run (await ∷ AWAIT String, base ∷ BaseEff (console ∷ CONSOLE | eff) | r) a
toConsole = forever (await >>= log >>> liftBase)

main ∷ Eff (console ∷ CONSOLE) Unit
main = runBase $ naturals !> take 100000 !> map show !> toConsole
