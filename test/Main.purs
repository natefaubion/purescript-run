module Test.Main where

import Prelude

import Control.Monad.Rec.Loops (whileM_)
import Data.Array as Array
import Data.Foldable (for_, oneOfMap)
import Effect (Effect)
import Effect.Console (logShow, log)
import Run (EFFECT, FProxy, Run, SProxy(..), lift, liftEffect, on, extract, runBaseEffect, run, send)
import Run.Choose (CHOOSE, runChoose)
import Run.Except (EXCEPT, runExcept, throw, catch)
import Run.State (STATE, runState, get, gets, put, modify)
import Test.Examples as Examples

data Talk a
  = Speak String a
  | Listen (String → a)

derive instance functorTalk ∷ Functor Talk

type TALK = FProxy Talk

_talk ∷ SProxy "talk"
_talk = SProxy

speak ∷ ∀ r. String → Run (talk ∷ TALK | r) Unit
speak a = lift _talk $ Speak a unit

listen ∷ ∀ r. Run (talk ∷ TALK | r) String
listen = lift _talk $ Listen identity

---

program ∷ ∀ r. String → Run (except ∷ EXCEPT String, state ∷ STATE String | r) Int
program a = do
  put "Hello"
  if a == "12"
    then put "World" $> 12
    else throw "Not 12"

program2 ∷ ∀ r. Run (state ∷ STATE Int, effect ∷ EFFECT | r) Int
program2 = do
  for_ (Array.range 1 100000) \n → do
    modify (_ + 1)
  liftEffect $ log "Done"
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
  , effect ∷ EFFECT
  )

yesProgram ∷ Run MyEffects Unit
yesProgram = do
  whenM (gets (_ < 0)) do
    throw "Number is less than 0"
  whileM_ (gets (_ > 0)) do
    liftEffect $ log "Yes"
    modify (_ - 1)

chooseProgram ∷ ∀ r. Run (choose ∷ CHOOSE, effect ∷ EFFECT | r) Int
chooseProgram = do
  n ← oneOfMap pure [1, 2, 3, 4, 5]
  liftEffect $ log $ show n
  pure (n + 1)

main ∷ Effect Unit
main = do
  program "42" # runState "" # runExcept # extract # logShow
  program "42" # runExcept # runState "" # extract # logShow
  program "12" # runState "" # runExcept # extract # logShow

  res1 ← program2 # runState 0 # runBaseEffect
  logShow res1

  let
    runSpeak = send # on _talk case _ of
      Speak str a  → liftEffect (log str) $> a
      Listen reply → pure $ reply "Gerald"

  program3
    # run runSpeak
    # runBaseEffect

  yesProgram
    # catch (liftEffect <<< log)
    # runState 10
    # runBaseEffect
    # void

  as ← chooseProgram
    # runChoose
    # runBaseEffect
  logShow (as ∷ Array Int)

  Examples.main >>= logShow
  Examples.mainSleep
