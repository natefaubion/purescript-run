module Run.Except
  ( Except(..)
  , EXCEPT
  , FAIL
  , _except
  , liftExcept
  , liftExceptAt
  , runExcept
  , runExceptAt
  , runFail
  , runFailAt
  , throw
  , throwAt
  , fail
  , failAt
  , rethrow
  , rethrowAt
  , note
  , noteAt
  , fromJust
  , fromJustAt
  , catch
  , catchAt
  ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe')
import Data.Symbol (class IsSymbol)
import Run (Run, SProxy(..), FProxy)
import Run as Run

newtype Except e a = Except e

derive instance functorExcept ∷ Functor (Except e)

type EXCEPT e = FProxy (Except e)

type FAIL = EXCEPT Unit

_except ∷ SProxy "except"
_except = SProxy

liftExcept ∷ ∀ e a r. Except e a → Run (except ∷ EXCEPT e | r) a
liftExcept = liftExceptAt _except

liftExceptAt
  ∷ ∀ t e a r s
  . IsSymbol s
  ⇒ RowCons s (EXCEPT e) t r
  ⇒ SProxy s
  → Except e a
  → Run r a
liftExceptAt = Run.lift

throw ∷ ∀ e a r. e → Run (except ∷ EXCEPT e | r) a
throw = throwAt _except

throwAt
  ∷ ∀ t e a r s
  . IsSymbol s
  ⇒ RowCons s (EXCEPT e) t r
  ⇒ SProxy s
  → e
  → Run r a
throwAt sym = liftExceptAt sym <<< Except

fail ∷ ∀ a r. Run (except ∷ FAIL | r) a
fail = failAt _except

failAt ∷
  ∀ t a r s
  . IsSymbol s
  ⇒ RowCons s FAIL t r
  ⇒ SProxy s
  → Run r a
failAt sym = throwAt sym unit

rethrow ∷ ∀ e a r. Either e a → Run (except ∷ EXCEPT e | r) a
rethrow = rethrowAt _except

rethrowAt ∷
  ∀ t e a r s
  . IsSymbol s
  ⇒ RowCons s (EXCEPT e) t r
  ⇒ SProxy s
  → Either e a
  → Run r a
rethrowAt sym = either (throwAt sym) pure

note ∷ ∀ e a r. e → Maybe a → Run (except ∷ EXCEPT e | r) a
note = noteAt _except

noteAt ∷
  ∀ t e a r s
  . IsSymbol s
  ⇒ RowCons s (EXCEPT e) t r
  ⇒ SProxy s
  → e
  → Maybe a
  → Run r a
noteAt sym e = maybe' (\_ → throwAt sym e) pure

fromJust ∷ ∀ a r. Maybe a → Run (except ∷ FAIL | r) a
fromJust = fromJustAt _except

fromJustAt ∷
  ∀ t a r s
  . IsSymbol s
  ⇒ RowCons s FAIL t r
  ⇒ SProxy s
  → Maybe a
  → Run r a
fromJustAt sym = noteAt sym unit

catch ∷ ∀ e a r. (e → Run r a) → Run (except ∷ EXCEPT e | r) a → Run r a
catch = catchAt _except

catchAt ∷
  ∀ t e a r s
  . IsSymbol s
  ⇒ RowCons s (EXCEPT e) t r
  ⇒ SProxy s
  → (e → Run t a)
  → Run r a
  → Run t a
catchAt sym = loop
  where
  handle = Run.on sym Left Right
  loop k r = case Run.peel r of
    Left a → case handle a of
      Left (Except e) →
        k e
      Right a' →
        Run.send a' >>= loop k
    Right a →
      pure a

runExcept ∷ ∀ e a r. Run (except ∷ EXCEPT e | r) a → Run r (Either e a)
runExcept = runExceptAt _except

runExceptAt ∷
  ∀ t e a r s
  . IsSymbol s
  ⇒ RowCons s (EXCEPT e) t r
  ⇒ SProxy s
  → Run r a
  → Run t (Either e a)
runExceptAt sym = loop
  where
  handle = Run.on sym Left Right
  loop r = case Run.peel r of
    Left a → case handle a of
      Left (Except e) →
        pure (Left e)
      Right a' →
        Run.send a' >>= loop
    Right a →
      pure (Right a)

runFail ∷ ∀ a r. Run (except ∷ FAIL | r) a → Run r (Maybe a)
runFail = runFailAt _except

runFailAt ∷
  ∀ t a r s
  . IsSymbol s
  ⇒ RowCons s FAIL t r
  ⇒ SProxy s
  → Run r a
  → Run t (Maybe a)
runFailAt sym = map (either (const Nothing) Just) <<< runExceptAt sym
