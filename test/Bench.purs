module Test.Bench where

import Prelude

import Control.Monad.Error.Class as EC
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.State.Class as SC
import Control.Monad.Trampoline (Trampoline, runTrampoline)
import Control.Monad.Writer (WriterT, runWriterT)
import Control.Monad.Writer.Class as WC
import Data.Identity (Identity(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (benchWith)
import Run (Run, extract)
import Run.Except (EXCEPT)
import Run.Except as RE
import Run.State (STATE)
import Run.State as RS
import Run.Writer (WRITER)
import Run.Writer as RW
import Type.Row (type (+))

type TestT = ExceptT String (WriterT String (StateT Int Trampoline))

type TestT' = ExceptT String (WriterT String (StateT Int Identity))

type TestR = Run (STATE Int + WRITER String + EXCEPT String + ())

test_mono :: TestT Int
test_mono = do
  x <- SC.get
  if x <= 0 then pure 0
  else test_inner
  where
  test_inner = do
    SC.modify_ (_ - 2)
    EC.catchError test_error WC.tell
    test_mono

  test_error = do
    SC.modify_ (_ + 1)
    x <- SC.get
    EC.throwError (show x)

test_mono' :: TestT' Int
test_mono' = do
  x <- SC.get
  if x <= 0 then pure 0
  else test_inner
  where
  test_inner = do
    SC.modify_ (_ - 2)
    EC.catchError test_error WC.tell
    test_mono'

  test_error = do
    SC.modify_ (_ + 1)
    x <- SC.get
    EC.throwError (show x)

test_mtl
  :: forall m
   . SC.MonadState Int m
  => WC.MonadWriter String m
  => EC.MonadError String m
  => m Int
test_mtl = do
  x <- SC.get
  if x <= 0 then pure 0
  else test_inner
  where
  test_inner = do
    SC.modify_ (_ - 2)
    EC.catchError test_error WC.tell
    test_mtl

  test_error = do
    SC.modify_ (_ + 1)
    x <- SC.get
    EC.throwError (show x)

test_run :: TestR Int
test_run = do
  x <- RS.get
  if x <= 0 then pure 0
  else test_inner
  where
  test_inner = do
    RS.modify (_ - 2)
    test_error # RE.catch RW.tell
    test_run

  test_error = do
    RS.modify (_ + 1)
    x <- RS.get
    RE.throw (show x)

main :: Effect Unit
main = do
  log "Transformers (monomorphic/trampoline)"
  benchWith 100 \_ ->
    test_mono
      # runExceptT
      # runWriterT
      # flip runStateT 1000
      # runTrampoline

  gc
  log "Transformers (monomorphic/identity)"
  benchWith 100 \_ ->
    test_mono'
      # runExceptT
      # runWriterT
      # flip runStateT 1000
      # un Identity

  gc
  log "Transformers (mtl/trampoline)"
  benchWith 100 \_ ->
    test_mtl
      # runExceptT
      # runWriterT
      # flip runStateT 1000
      # runTrampoline

  gc
  log "Transformers (mtl/identity)"
  benchWith 100 \_ ->
    test_mtl
      # runExceptT
      # runWriterT
      # flip runStateT 1000
      # un Identity

  gc
  log "Run (free)"
  benchWith 100 \_ ->
    test_run
      # RE.runExcept
      # RW.runWriter
      # RS.runState 1000
      # extract

foreign import gc :: Effect Unit
