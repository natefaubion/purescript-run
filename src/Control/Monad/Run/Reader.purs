module Control.Monad.Run.Reader
  ( Reader(..)
  , READER
  , _READER
  , liftReader
  , ask
  , local
  , runReader
  ) where

import Prelude
import Control.Monad.Run (Run, REffect, RProxy(..), liftEffect, peel, send, decomp)
import Data.Either (Either(..))

newtype Reader e a = Reader (e → a)

derive newtype instance functorReader ∷ Functor (Reader e)

type READER e = REffect (Reader e)

_READER ∷ ∀ e. RProxy "reader" (Reader e)
_READER = RProxy

liftReader ∷ ∀ e a r. Reader e a → Run (reader ∷ READER e | r) a
liftReader = liftEffect _READER

ask ∷ ∀ e r. Run (reader ∷ READER e | r) e
ask = liftReader (Reader id)

local ∷ ∀ e a r. (e → e) → Run (reader ∷ READER e | r) a → Run (reader ∷ READER e | r) a
local = \f r → map f ask >>= flip runLocal r
  where
  handle = decomp _READER
  runLocal = loop
    where
    loop e r = case peel r of
      Left a → case handle a of
        Left a' →
          send a >>= runLocal e
        Right (Reader k) →
          loop e (k e)
      Right a →
        pure a

runReader ∷ ∀ e a r. e → Run (reader ∷ READER e | r) a → Run r a
runReader = loop
  where
  handle = decomp _READER
  loop e r = case peel r of
    Left a → case handle a of
      Left a' →
        send a' >>= runReader e
      Right (Reader k) →
        loop e (k e)
    Right a →
      pure a
