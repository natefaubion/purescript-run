module Test.Examples where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Data.Tuple (Tuple(..))
import Run (EFFECT, FProxy, Run, SProxy(..), Step(..), interpret, liftEffect, match, on, runAccumPure, runBaseEffect, runCont, send)
import Run as Run

data TalkF a
  = Speak String a
  | Listen (String -> a)

derive instance functorTalkF :: Functor TalkF

type TALK = FProxy TalkF

_talk = SProxy :: SProxy "talk"

speak :: forall r. String -> Run (talk :: TALK | r) Unit
speak str = Run.lift _talk (Speak str unit)

listen :: forall r. Run (talk :: TALK | r) String
listen = Run.lift _talk (Listen identity)

handleTalk :: forall r. TalkF ~> Run (effect :: EFFECT | r)
handleTalk = case _ of
  Speak str next -> do
    liftEffect $ Console.log str
    pure next
  Listen reply -> do
    pure (reply "I am Groot")

runTalk
  :: forall r
   . Run (effect :: EFFECT, talk :: TALK | r)
  ~> Run (effect :: EFFECT | r)
runTalk = interpret (on _talk handleTalk send)

---

type IsThereMore = Boolean
type Bill = Int

data Food = Pizza | Chizburger

data DinnerF a
  = Eat Food (IsThereMore -> a)
  | CheckPlease (Bill -> a)

derive instance functorDinnerF :: Functor DinnerF

type DINNER = FProxy DinnerF

_dinner = SProxy :: SProxy "dinner"

eat :: forall r. Food -> Run (dinner :: DINNER | r) IsThereMore
eat food = Run.lift _dinner (Eat food identity)

checkPlease :: forall r. Run (dinner :: DINNER | r) Bill
checkPlease = Run.lift _dinner (CheckPlease identity)

type Tally = { stock :: Int, bill :: Bill }

handleDinner :: forall a. Tally -> DinnerF a -> Tuple Tally a
handleDinner tally = case _ of
  Eat _ reply
    | tally.stock > 0 ->
        let tally' = { stock: tally.stock - 1, bill: tally.bill + 1 }
        in Tuple tally' (reply true)
    | otherwise ->
        Tuple tally (reply false)
  CheckPlease reply ->
    Tuple tally (reply tally.bill)

runDinnerPure :: forall r a. Tally -> Run (dinner :: DINNER | r) a -> Run r (Tuple Bill a)
runDinnerPure = runAccumPure
  (\tally -> on _dinner (Loop <<< handleDinner tally) Done)
  (\tally a -> Tuple tally.bill a)

---

type LovelyEvening r = (talk :: TALK, dinner :: DINNER | r)

dinnerTime :: forall r. Run (LovelyEvening r) Unit
dinnerTime = do
  speak "I'm famished!"
  isThereMore <- eat Pizza
  if isThereMore
    then dinnerTime
    else do
      bill <- checkPlease
      speak "Outrageous!"

program2 :: forall r. Run (effect :: EFFECT, dinner :: DINNER | r) Unit
program2 = dinnerTime # runTalk

program3 :: forall r. Run (effect :: EFFECT | r) (Tuple Bill Unit)
program3 = program2 # runDinnerPure { stock: 10, bill: 0 }

main :: Effect (Tuple Bill Unit)
main = runBaseEffect program3

---

foreign import setTimeout :: Int -> Effect Unit -> Effect Unit

---

data LogF a = Log String a

derive instance functorLogF :: Functor LogF

type LOG = FProxy LogF

_log = SProxy :: SProxy "log"

log :: forall r. String -> Run (log :: LOG | r) Unit
log str = Run.lift _log (Log str unit)

---

data SleepF a = Sleep Int a

derive instance functorSleepF :: Functor SleepF

type SLEEP = FProxy SleepF

_sleep = SProxy :: SProxy "sleep"

sleep :: forall r. Int -> Run (sleep :: SLEEP | r) Unit
sleep ms = Run.lift _sleep (Sleep ms unit)

---

programSleep :: forall r. Run (sleep :: SLEEP, log :: LOG | r) Unit
programSleep = do
  log "I guess I'll wait..."
  sleep 3000
  log "I can't wait any longer!"

mainSleep :: Effect Unit
mainSleep = programSleep # runCont go done
  where
  go = match
    { log: \(Log str cb) -> Console.log str *> cb
    , sleep: \(Sleep ms cb) -> setTimeout ms cb
    }

  done _ = do
    Console.log "Done!"
