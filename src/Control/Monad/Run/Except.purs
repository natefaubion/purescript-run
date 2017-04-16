module Control.Monad.Run.Except
  ( Except
  , EXCEPT
  , FAIL
  , _EXCEPT
  , liftExcept
  , runExcept
  , runFail
  , throw
  , fail
  , catch
  ) where

import Prelude
import Control.Monad.Run (Run, REffect, RProxy(..), liftEffect, peel, send, project)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))

newtype Except e a = Except e

derive instance functorExcept ∷ Functor (Except e)

type EXCEPT e = REffect (Except e)

type FAIL = EXCEPT Unit

_EXCEPT ∷ ∀ e. RProxy "except" (Except e)
_EXCEPT = RProxy

liftExcept ∷ ∀ e a r. Except e a → Run (except ∷ EXCEPT e | r) a
liftExcept = liftEffect _EXCEPT

throw ∷ ∀ e a r. e → Run (except ∷ EXCEPT e | r) a
throw = liftExcept <<< Except

fail ∷ ∀ a r. Run (except ∷ FAIL | r) a
fail = throw unit

catch ∷ ∀ e a r. (e → Run r a) → Run (except ∷ EXCEPT e | r) a → Run r a
catch = loop
  where
  handle = project _EXCEPT
  loop k r = case peel r of
    Left a → case handle a of
      Left a' →
        send a' >>= catch k
      Right (Except e) →
        k e
    Right a →
      pure a

runExcept ∷ ∀ e a r. Run (except ∷ EXCEPT e | r) a → Run r (Either e a)
runExcept = loop
  where
  handle = project _EXCEPT
  loop r = case peel r of
    Left a → case handle a of
      Left a' →
        send a' >>= runExcept
      Right (Except e) →
        pure (Left e)
    Right a →
      pure (Right a)

runFail ∷ ∀ a r. Run (except ∷ FAIL | r) a → Run r (Maybe a)
runFail = map (either (const Nothing) Just) <<< runExcept
