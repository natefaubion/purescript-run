module Test.Main where

import Prelude

import Control.Monad.Rec.Class (tailRecM, Step(..))
import Data.Array as Array
import Data.Foldable (for_, oneOfMap)
import Data.Monoid.Additive (Additive(..))
import Effect (Effect)
import Effect.Console (logShow, log)
import Run (EFFECT, FProxy, Run, SProxy(..), lift, liftEffect, on, extract, runBaseEffect, run, send)
import Run.Choose (CHOOSE, runChoose)
import Run.Except (EXCEPT, catch, runExcept, runExceptAt, throw, throwAt)
import Run.Reader (READER, ask, runReader)
import Run.State (STATE, get, gets, modify, put, putAt, runState, runStateAt)
import Run.Writer (WRITER, runWriter, tell)
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

program4 ∷ ∀ r. String → Run (exc ∷ EXCEPT String, st ∷ STATE String | r) Int
program4 a = do
  putAt _st "Hello"
  if a == "12"
    then putAt _st "World" $> 12
    else throwAt _exc "Not 12"

_exc ∷ SProxy "exc"
_exc = SProxy

_st ∷ SProxy "st"
_st = SProxy

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
  where
  whileM_
    ∷ ∀ a
    . Run MyEffects Boolean
    → Run MyEffects a
    → Run MyEffects Unit
  whileM_ mb ma = flip tailRecM unit \a →
    mb >>= if _ then ma $> Loop unit else pure $ Done unit

chooseProgram ∷ ∀ r. Run (choose ∷ CHOOSE, effect ∷ EFFECT | r) Int
chooseProgram = do
  n ← oneOfMap pure [1, 2, 3, 4, 5]
  liftEffect $ log $ show n
  pure (n + 1)

tcoLoop ∷ ∀ r. Int -> (Int -> Run r Unit) -> Run r Unit
tcoLoop n k = go n
  where
  go n'
    | n' == 0 = pure unit
    | otherwise = do
        k n'
        go (n' - 1)

stateTCO ∷ ∀ r. Run (state ∷ STATE Int | r) Unit
stateTCO = tcoLoop 100000 put

writerTCO ∷ ∀ r. Run (writer ∷ WRITER (Additive Int) | r) Unit
writerTCO = tcoLoop 100000 \_ -> tell (Additive 1)

readerTCO ∷ ∀ r. Run (reader ∷ READER Unit | r) Unit
readerTCO = tcoLoop 100000 (const ask)

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

  program4 "42" # runStateAt _st "" # runExceptAt _exc # extract # logShow
  program4 "42" # runExceptAt _exc # runStateAt _st "" # extract # logShow
  program4 "12" # runStateAt _st "" # runExceptAt _exc # extract # logShow

  yesProgram
    # catch (liftEffect <<< log)
    # runState 10
    # runBaseEffect
    # void

  as ← chooseProgram
    # runChoose
    # runBaseEffect
  logShow (as ∷ Array Int)

  let
    tco1 = stateTCO # runState 0
    tco2 = writerTCO # runWriter
    tco3 = readerTCO # runReader unit

  Examples.main >>= logShow
  Examples.mainSleep
