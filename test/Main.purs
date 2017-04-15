module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Run (Run, run, runBase, BaseEff)
import Control.Monad.Run.Except (EXCEPT, runExcept, throw)
import Control.Monad.Run.State (STATE, runState, get, put, modify)
import Data.Array as Array
import Data.Foldable (for_)

program ∷ String → Run (except ∷ EXCEPT String, state ∷ STATE String) Int
program a = do
  put "Hello"
  if a == "12"
    then put "World" $> 12
    else throw "Not 12"

program2 ∷ Run (state ∷ STATE Int, base ∷ BaseEff (console ∷ CONSOLE)) Int
program2 = do
  for_ (Array.range 0 100000) \n → do
    modify (_ + n)
    liftEff $ logShow n
  get

main ∷ Eff (console ∷ CONSOLE) Unit
main = do
  logShow $ run $ runExcept $ runState "" $ program "42"
  logShow $ run $ runState "" $ runExcept $ program "42"
  logShow $ run $ runExcept $ runState "" $ program "12"
  logShow =<< runBase (runState 0 program2)
