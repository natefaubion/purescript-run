module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow, log)
import Control.Monad.Rec.Loops (whileM_)
import Data.Array as Array
import Data.Foldable (for_)
import Run (Run, FProxy, SProxy(..), liftEffect, liftBase, runWithBase, run, runBase, BaseEff)
import Run.Except (EXCEPT, runExcept, throw, catch)
import Run.State (STATE, runState, get, gets, put, modify)

data Talk a
  = Speak String a
  | Listen (String → a)

derive instance functorTalk ∷ Functor Talk

type TALK = FProxy Talk

_talk ∷ SProxy "talk"
_talk = SProxy

speak ∷ ∀ r. String → Run (talk ∷ TALK | r) Unit
speak a = liftEffect _talk $ Speak a unit

listen ∷ ∀ r. Run (talk ∷ TALK | r) String
listen = liftEffect _talk $ Listen id

---

program ∷ ∀ r. String → Run (except ∷ EXCEPT String, state ∷ STATE String | r) Int
program a = do
  put "Hello"
  if a == "12"
    then put "World" $> 12
    else throw "Not 12"

program2 ∷ ∀ r. Run (state ∷ STATE Int, base ∷ BaseEff (console ∷ CONSOLE) | r) Int
program2 = do
  for_ (Array.range 0 100000) \n → do
    modify (_ + 1)
  liftEff $ log "Done"
  get

program3 ∷ ∀ r. Run (talk ∷ TALK | r) Unit
program3 = do
  speak "Hello, there."
  speak "What is your name?"
  name ← listen
  speak $ "Nice to meet you, " <> name <> "!"

type MyEffects =
  ( state ∷ STATE Int
  , except ∷ EXCEPT String
  , base ∷ BaseEff (console ∷ CONSOLE)
  )

yesProgram ∷ Run MyEffects Unit
yesProgram = do
  whenM (gets (_ < 0)) do
    throw "Number is less than 0"
  whileM_ (gets (_ > 0)) do
    liftBase $ log "Yes"
    modify (_ - 1)

main ∷ Eff (console ∷ CONSOLE) Unit
main = do
  program "42" # runState "" # runExcept # run # logShow
  program "42" # runExcept # runState "" # run # logShow
  program "12" # runState "" # runExcept # run # logShow

  res1 ← program2 # runState 0 # runBase
  logShow res1

  program3
    # runWithBase _talk case _ of
        Speak str a  → log str $> a
        Listen reply → pure $ reply "Gerald"
    # runBase

  yesProgram
    # catch (liftBase <<< log)
    # runState 10
    # runBase
    # void
