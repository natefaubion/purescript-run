module Control.Monad.Run.State
  ( State(..)
  , STATE
  , _STATE
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
import Control.Monad.Run (Run, REffect, RProxy(..), liftEffect, peel, send, decomp)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst, snd)

data State s a = State (s → s) (s → a)

derive instance functorState ∷ Functor (State s)

type STATE s = REffect (State s)

_STATE ∷ ∀ s. RProxy "state" (State s)
_STATE = RProxy

liftState ∷ ∀ s a r. State s a → Run (state ∷ STATE s | r) a
liftState = liftEffect _STATE

modify ∷ ∀ s r. (s → s) → Run (state ∷ STATE s | r) Unit
modify f = liftState $ State f (const unit)

put ∷ ∀ s r. s → Run (state ∷ STATE s | r) Unit
put = modify <<< const

get ∷ ∀ s r. Run (state ∷ STATE s | r) s
get = liftState $ State id id

gets ∷ ∀ s t r. (s → t) → Run (state ∷ STATE s | r) t
gets = flip map get

runState ∷ ∀ s r a. s → Run (state ∷ STATE s | r) a → Run r (Tuple s a)
runState = loop
  where
  handle = decomp _STATE
  loop s r = case peel r of
    Left a → case handle a of
      Left a' →
        send a' >>= runState s
      Right (State t k) →
        let s' = t s
        in loop s' (k s')
    Right a →
      pure (Tuple s a)

evalState ∷ ∀ s r a. s → Run (state ∷ STATE s | r) a → Run r a
evalState s = map snd <<< runState s

execState ∷ ∀ s r a. s → Run (state ∷ STATE s | r) a → Run r s
execState s = map fst <<< runState s
