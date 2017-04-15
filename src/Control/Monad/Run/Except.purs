module Control.Monad.Run.Except
  ( Except
  , EXCEPT
  , FAIL
  , _EXCEPT
  , liftExcept
  , runExcept
  , runFail
  , throw
  , catch
  ) where

import Prelude
import Control.Monad.Run (Run, REffect, RProxy(..), liftRun, relay, peel)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))

newtype Except e a = Except e

derive instance functorExcept ∷ Functor (Except e)

type EXCEPT e = REffect (Except e)

type FAIL = EXCEPT Unit

_EXCEPT ∷ ∀ e. RProxy "except" (Except e)
_EXCEPT = RProxy

liftExcept ∷ ∀ e a r. Except e a → Run (except ∷ EXCEPT e | r) a
liftExcept = liftRun _EXCEPT

throw ∷ ∀ e a r. e → Run (except ∷ EXCEPT e | r) a
throw = liftExcept <<< Except

catch ∷ ∀ e a r. (e → Run r a) → Run (except ∷ EXCEPT e | r) a → Run r a
catch = loop
  where
  handle = relay _EXCEPT
  loop k r = case peel r of
    Left a  → handle (loop k) (\(Except e) → k e) a
    Right a → pure a

runExcept ∷ ∀ e a r. Run (except ∷ EXCEPT e | r) a → Run r (Either e a)
runExcept = loop
  where
  handle = relay _EXCEPT
  loop r = case peel r of
    Left a  → handle loop (\(Except e) → pure (Left e)) a
    Right a → pure (Right a)

runFail ∷ ∀ a r. Run (except ∷ FAIL | r) a → Run r (Maybe a)
runFail = map (either (const Nothing) Just) <<< runExcept
