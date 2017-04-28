module Run.Writer
  ( Writer(..)
  , WRITER
  , _writer
  , liftWriter
  , tell
  , censor
  , foldWriter
  , runWriter
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))
import Run (Run, SProxy(..), FProxy)
import Run as Run

data Writer w a = Writer w a

derive instance functorWriter ∷ Functor (Writer w)

type WRITER w = FProxy (Writer w)

_writer ∷ SProxy "writer"
_writer = SProxy

liftWriter ∷ ∀ w a r. Writer w a → Run (writer ∷ WRITER w | r) a
liftWriter = Run.liftEffect _writer

tell ∷ ∀ w r. w → Run (writer ∷ WRITER w | r) Unit
tell w = liftWriter (Writer w unit)

censor ∷ ∀ w a r. (w → w) → Run (writer ∷ WRITER w | r) a → Run (writer ∷ WRITER w | r) a
censor = loop
  where
  handle = Run.on _writer Left Right
  loop f r = case Run.peel r of
    Left a → case handle a of
      Left (Writer w n) → do
        tell (f w)
        censor f n
      Right _ →
        Run.send a >>= censor f
    Right a →
      pure a

foldWriter ∷ ∀ w b a r. (b → w → b) → b → Run (writer ∷ WRITER w | r) a → Run r (Tuple b a)
foldWriter = loop
  where
  handle = Run.on _writer Left Right
  loop k w r = case Run.peel r of
    Left a → case handle a of
      Left (Writer w' n) →
        loop k (k w w') n
      Right a' →
        Run.send a' >>= foldWriter k w
    Right a →
      pure (Tuple w a)

runWriter ∷ ∀ w a r. Monoid w ⇒ Run (writer ∷ WRITER w | r) a → Run r (Tuple w a)
runWriter = foldWriter (<>) mempty
