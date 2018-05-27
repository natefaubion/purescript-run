module Run.State
  ( State(..)
  , STATE
  , _state
  , liftState
  , liftStateAt
  , modify
  , modifyAt
  , put
  , putAt
  , get
  , getAt
  , gets
  , getsAt
  , runState
  , runStateAt
  , evalState
  , evalStateAt
  , execState
  , execStateAt
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Run (Run, SProxy(..), FProxy)
import Run as Run

data State s a = State (s → s) (s → a)

derive instance functorState ∷ Functor (State s)

type STATE s = FProxy (State s)

_state ∷ SProxy "state"
_state = SProxy

liftState ∷ ∀ s a r. State s a → Run (state ∷ STATE s | r) a
liftState = liftStateAt _state

liftStateAt ∷
  ∀ q sym s a r
  . IsSymbol sym
  ⇒ RowCons sym (STATE s) q r
  ⇒ SProxy sym
  → State s a
  → Run r a
liftStateAt = Run.lift

modify ∷ ∀ s r. (s → s) → Run (state ∷ STATE s | r) Unit
modify = modifyAt _state

modifyAt ∷
  ∀ q sym s r
  . IsSymbol sym
  ⇒ RowCons sym (STATE s) q r
  ⇒ SProxy sym
  → (s → s)
  → Run r Unit
modifyAt sym f = liftStateAt sym $ State f (const unit)

put ∷ ∀ s r. s → Run (state ∷ STATE s | r) Unit
put = putAt _state

putAt ∷
  ∀ q sym s r
  . IsSymbol sym
  ⇒ RowCons sym (STATE s) q r
  ⇒ SProxy sym
  → s
  → Run r Unit
putAt sym = modifyAt sym <<< const

get ∷ ∀ s r. Run (state ∷ STATE s | r) s
get = getAt _state

getAt ∷
  ∀ q sym s r
  . IsSymbol sym
  ⇒ RowCons sym (STATE s) q r
  ⇒ SProxy sym
  → Run r s
getAt sym = liftStateAt sym $ State id id

gets ∷ ∀ s t r. (s → t) → Run (state ∷ STATE s | r) t
gets = getsAt _state

getsAt ∷
  ∀ q sym s t r
  . IsSymbol sym
  ⇒ RowCons sym (STATE s) q r
  ⇒ SProxy sym
  → (s → t)
  → Run r t
getsAt sym = flip map (getAt sym)

runState ∷ ∀ s r a. s → Run (state ∷ STATE s | r) a → Run r (Tuple s a)
runState = runStateAt _state

runStateAt ∷
  ∀ q sym s r a
  . IsSymbol sym
  ⇒ RowCons sym (STATE s) q r
  ⇒ SProxy sym
  → s
  → Run r a
  → Run q (Tuple s a)
runStateAt sym = loop
  where
  handle = Run.on sym Left Right
  loop s r = case Run.peel r of
    Left a → case handle a of
      Left (State t k) →
        let s' = t s
        in loop s' (k s')
      Right a' →
        Run.send a' >>= runStateAt sym s
    Right a →
      pure (Tuple s a)

evalState ∷ ∀ s r a. s → Run (state ∷ STATE s | r) a → Run r a
evalState = evalStateAt _state

evalStateAt ∷
  ∀ q sym s r a
  . IsSymbol sym
  ⇒ RowCons sym (STATE s) q r
  ⇒ SProxy sym
  → s
  → Run r a
  → Run q a
evalStateAt sym s = map snd <<< runStateAt sym s

execState ∷ ∀ s r a. s → Run (state ∷ STATE s | r) a → Run r s
execState = execStateAt _state

execStateAt ∷
  ∀ q sym s r a
  . IsSymbol sym
  ⇒ RowCons sym (STATE s) q r
  ⇒ SProxy sym
  → s
  → Run r a
  → Run q s
execStateAt sym s = map fst <<< runStateAt sym s
