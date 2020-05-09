module Run.Reader
  ( Reader(..)
  , READER
  , _reader
  , liftReader
  , liftReaderAt
  , ask
  , askAt
  , local
  , localAt
  , runReader
  , runReaderAt
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Run (Run, SProxy(..), FProxy)
import Run as Run

newtype Reader e a = Reader (e → a)

derive newtype instance functorReader ∷ Functor (Reader e)

type READER e = FProxy (Reader e)

_reader ∷ SProxy "reader"
_reader = SProxy

liftReader ∷ ∀ e a r. Reader e a → Run (reader ∷ READER e | r) a
liftReader = liftReaderAt _reader

liftReaderAt ∷
  ∀ t e a r s
  . IsSymbol s
  ⇒ Row.Cons s (READER e) t r
  ⇒ SProxy s
  → Reader e a
  → Run r a
liftReaderAt = Run.lift

ask ∷ ∀ e r. Run (reader ∷ READER e | r) e
ask = askAt _reader

askAt ∷
  ∀ t e r s
  . IsSymbol s
  ⇒ Row.Cons s (READER e) t r
  ⇒ SProxy s
  → Run r e
askAt sym = asksAt sym identity

asks ∷ ∀ e r a. (e → a) → Run (reader ∷ READER e | r) a
asks = asksAt _reader

asksAt ∷
  ∀ t e r s a
  . IsSymbol s
  ⇒ Row.Cons s (READER e) t r
  ⇒ SProxy s
  → (e → a)
  → Run r a
asksAt sym f = liftReaderAt sym (Reader f)

local ∷ ∀ e a r. (e → e) → Run (reader ∷ READER e | r) a → Run (reader ∷ READER e | r) a
local = localAt _reader

localAt ∷
  ∀ t e a r s
  . IsSymbol s
  ⇒ Row.Cons s (READER e) t r
  ⇒ SProxy s
  → (e → e)
  → Run r a
  → Run r a
localAt sym = \f r → map f (askAt sym) >>= flip runLocal r
  where
  handle = Run.on sym Left Right
  runLocal = loop
    where
    loop e r = case Run.peel r of
      Left a → case handle a of
        Left (Reader k) →
          loop e (k e)
        Right _ →
          Run.send a >>= runLocal e
      Right a →
        pure a

runReader ∷ ∀ e a r. e → Run (reader ∷ READER e | r) a → Run r a
runReader = runReaderAt _reader

runReaderAt ∷
  ∀ t e a r s
  . IsSymbol s
  ⇒ Row.Cons s (READER e) t r
  ⇒ SProxy s
  → e
  → Run r a
  → Run t a
runReaderAt sym = loop
  where
  handle = Run.on sym Left Right
  loop e r = case Run.peel r of
    Left a → case handle a of
      Left (Reader k) →
        loop e (k e)
      Right a' →
        Run.send a' >>= runReaderAt sym e
    Right a →
      pure a
