module Run.State
  ( State(..)
  , STATE
  , _state
  , liftState
  , modify
  , put
  , get
  , gets
  , runState
  , evalState
  , execState
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst, snd)
import Run (Run, SProxy(..), FProxy)
import Run as Run

data State s a = State (s → s) (s → a)

derive instance functorState ∷ Functor (State s)

type STATE s = FProxy (State s)

_state ∷ SProxy "state"
_state = SProxy

liftState ∷ ∀ s a r. State s a → Run (state ∷ STATE s | r) a
liftState = Run.lift _state

modify ∷ ∀ s r. (s → s) → Run (state ∷ STATE s | r) Unit
modify f = liftState $ State f (const unit)

put ∷ ∀ s r. s → Run (state ∷ STATE s | r) Unit
put = modify <<< const

get ∷ ∀ s r. Run (state ∷ STATE s | r) s
get = liftState $ State identity identity

gets ∷ ∀ s t r. (s → t) → Run (state ∷ STATE s | r) t
gets = flip map get

runState ∷ ∀ s r a. s → Run (state ∷ STATE s | r) a → Run r (Tuple s a)
runState = loop
  where
  handle = Run.on _state Left Right
  loop s r = case Run.peel r of
    Left a → case handle a of
      Left (State t k) →
        let s' = t s
        in loop s' (k s')
      Right a' →
        Run.send a' >>= runState s
    Right a →
      pure (Tuple s a)

evalState ∷ ∀ s r a. s → Run (state ∷ STATE s | r) a → Run r a
evalState s = map snd <<< runState s

execState ∷ ∀ s r a. s → Run (state ∷ STATE s | r) a → Run r s
execState s = map fst <<< runState s
