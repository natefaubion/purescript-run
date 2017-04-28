module Control.Monad.Run.Reader
  ( Reader(..)
  , READER
  , _reader
  , liftReader
  , ask
  , local
  , runReader
  ) where

import Prelude
import Control.Monad.Run (Run, SProxy(..), FProxy)
import Control.Monad.Run as Run
import Data.Either (Either(..))

newtype Reader e a = Reader (e → a)

derive newtype instance functorReader ∷ Functor (Reader e)

type READER e = FProxy (Reader e)

_reader ∷ SProxy "reader"
_reader = SProxy

liftReader ∷ ∀ e a r. Reader e a → Run (reader ∷ READER e | r) a
liftReader = Run.liftEffect _reader

ask ∷ ∀ e r. Run (reader ∷ READER e | r) e
ask = liftReader (Reader id)

local ∷ ∀ e a r. (e → e) → Run (reader ∷ READER e | r) a → Run (reader ∷ READER e | r) a
local = \f r → map f ask >>= flip runLocal r
  where
  handle = Run.on _reader Left Right
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
runReader = loop
  where
  handle = Run.on _reader Left Right
  loop e r = case Run.peel r of
    Left a → case handle a of
      Left (Reader k) →
        loop e (k e)
      Right a' →
        Run.send a' >>= runReader e
    Right a →
      pure a
