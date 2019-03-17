module Run.Choose
  ( liftChoose
  , calt
  , runChoose
  , module Run.Internal
  ) where

import Prelude

import Control.Alt (class Alt, alt)
import Data.Either (Either(..))
import Run (Run)
import Run as Run
import Run.Internal (Choose(..), CHOOSE, _choose)

liftChoose ∷ ∀ r a. Choose a → Run (choose ∷ CHOOSE | r) a
liftChoose = Run.lift _choose

calt ∷ ∀ r a. Run (choose ∷ CHOOSE | r) a → Run (choose ∷ CHOOSE | r) a → Run (choose ∷ CHOOSE | r) a
calt = alt

runChoose ∷ ∀ f a r. Alt f => Applicative f ⇒ Run (choose ∷ CHOOSE | r) a → Run r (f a)
runChoose = loop
  where
  handle = Run.on _choose Left Right
  loop r = case Run.peel r of
    Left a → case handle a of
      Left a' → case a' of
        Alt k → do
          x ← loop (k true)
          y ← loop (k false)
          pure (alt x y)
      Right a' →
        Run.send a' >>= loop
    Right a →
        pure (pure a)
