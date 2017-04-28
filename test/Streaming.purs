module Test.Streaming where

import Prelude hiding (map)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Run (Run, runBase, liftBase, BaseEff)
import Run.Streaming (Producer, Consumer, Transformer, (!>), yield, await)

forever ∷ ∀ r a b. Run r a → Run r b
forever go = go >>= \_ → forever go

map ∷ ∀ x y r a. (x → y) → Transformer r x y a
map f = forever (await >>= f >>> yield)

take ∷ ∀ x r. Int → Transformer r x x Unit
take n
  | n <= 0    = pure unit
  | otherwise = do
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

main ∷ Eff (console ∷ CONSOLE) Unit
main = runBase $
  naturals
  !> take 10
  !> map (show >>> append "Stream: ")
  !> toConsole
