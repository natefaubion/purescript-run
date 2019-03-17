module Run.Empty where

import Prelude

import Control.Plus (class Plus, empty)
import Data.Either (Either(..))
import Run (Run)
import Run as Run
import Run.Internal (Empty(..), EMPTY, _empty)

liftEmpty :: forall r a. Empty a -> Run (empty :: EMPTY | r) a
liftEmpty = Run.lift _empty

runEmpty :: forall f a r. Applicative f => Plus f => Run (empty :: EMPTY | r) a -> Run r (f a)
runEmpty = loop
  where
  handle = Run.on _empty Left Right
  loop r = case Run.peel r of
    Left a -> case handle a of
      Left a' -> case a' of
        Empty -> pure empty
      Right a' ->
        Run.send a' >>= loop
    Right a ->
      pure (pure a)
