module Control.Monad.Run.Writer
  ( Writer(..)
  , WRITER
  , _WRITER
  , liftWriter
  , tell
  , censor
  , foldWriter
  , runWriter
  ) where

import Prelude
import Control.Monad.Run (Run, REffect, RProxy(..), liftEffect, peel, send, decomp)
import Data.Either (Either(..))
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))

data Writer w a = Writer w a

derive instance functorWriter ∷ Functor (Writer w)

type WRITER w = REffect (Writer w)

_WRITER ∷ ∀ w. RProxy "writer" (Writer w)
_WRITER = RProxy

liftWriter ∷ ∀ w a r. Writer w a → Run (writer ∷ WRITER w | r) a
liftWriter = liftEffect _WRITER

tell ∷ ∀ w r. w → Run (writer ∷ WRITER w | r) Unit
tell w = liftWriter (Writer w unit)

censor ∷ ∀ w a r. (w → w) → Run (writer ∷ WRITER w | r) a → Run (writer ∷ WRITER w | r) a
censor = loop
  where
  handle = decomp _WRITER
  loop f r = case peel r of
    Left a → case handle a of
      Left _ →
        send a >>= censor f
      Right (Writer w n) → do
        tell (f w)
        censor f n
    Right a →
      pure a

foldWriter ∷ ∀ w b a r. (b → w → b) → b → Run (writer ∷ WRITER w | r) a → Run r (Tuple b a)
foldWriter = loop
  where
  handle = decomp _WRITER
  loop k w r = case peel r of
    Left a → case handle a of
      Left a' →
        send a' >>= foldWriter k w
      Right (Writer w' n) →
        loop k (k w w') n
    Right a →
      pure (Tuple w a)

runWriter ∷ ∀ w a r. Monoid w ⇒ Run (writer ∷ WRITER w | r) a → Run r (Tuple w a)
runWriter = foldWriter (<>) mempty
