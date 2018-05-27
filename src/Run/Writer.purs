module Run.Writer
  ( Writer(..)
  , WRITER
  , _writer
  , liftWriter
  , liftWriterAt
  , tell
  , tellAt
  , censor
  , censorAt
  , foldWriter
  , foldWriterAt
  , runWriter
  , runWriterAt
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Monoid (class Monoid, mempty)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Run (Run, SProxy(..), FProxy)
import Run as Run

data Writer w a = Writer w a

derive instance functorWriter ∷ Functor (Writer w)

type WRITER w = FProxy (Writer w)

_writer ∷ SProxy "writer"
_writer = SProxy

liftWriter ∷ ∀ w a r. Writer w a → Run (writer ∷ WRITER w | r) a
liftWriter = liftWriterAt _writer

liftWriterAt ∷
  ∀ w a r t s
  . IsSymbol s
  ⇒ RowCons s (WRITER w) t r
  ⇒ SProxy s
  → Writer w a
  → Run r a
liftWriterAt = Run.lift

tell ∷ ∀ w r. w → Run (writer ∷ WRITER w | r) Unit
tell = tellAt _writer

tellAt ∷
  ∀ w r t s
  . IsSymbol s
  ⇒ RowCons s (WRITER w) t r
  ⇒ SProxy s
  → w
  → Run r Unit
tellAt sym w = liftWriterAt sym (Writer w unit)

censor ∷ ∀ w a r. (w → w) → Run (writer ∷ WRITER w | r) a → Run (writer ∷ WRITER w | r) a
censor = censorAt _writer

censorAt ∷
  ∀ w a r t s
  . IsSymbol s
  ⇒ RowCons s (WRITER w) t r
  ⇒ SProxy s
  → (w → w)
  → Run r a
  → Run r a
censorAt sym = loop
  where
  handle = Run.on sym Left Right
  loop f r = case Run.peel r of
    Left a → case handle a of
      Left (Writer w n) → do
        tellAt sym (f w)
        censorAt sym f n
      Right _ →
        Run.send a >>= censorAt sym f
    Right a →
      pure a

foldWriter ∷ ∀ w b a r. (b → w → b) → b → Run (writer ∷ WRITER w | r) a → Run r (Tuple b a)
foldWriter = foldWriterAt _writer

foldWriterAt ∷
  ∀ w b a r t s
  . IsSymbol s
  ⇒ RowCons s (WRITER w) t r
  ⇒ SProxy s
  → (b → w → b)
  → b
  → Run r a
  → Run t (Tuple b a)
foldWriterAt sym = loop
  where
  handle = Run.on sym Left Right
  loop k w r = case Run.peel r of
    Left a → case handle a of
      Left (Writer w' n) →
        loop k (k w w') n
      Right a' →
        Run.send a' >>= foldWriterAt sym k w
    Right a →
      pure (Tuple w a)

runWriter ∷ ∀ w a r. Monoid w ⇒ Run (writer ∷ WRITER w | r) a → Run r (Tuple w a)
runWriter = runWriterAt _writer

runWriterAt ∷
  ∀ w a r t s
  . IsSymbol s
  ⇒ Monoid w
  ⇒ RowCons s (WRITER w) t r
  ⇒ SProxy s
  → Run r a
  → Run t (Tuple w a)
runWriterAt sym = foldWriterAt sym (<>) mempty
