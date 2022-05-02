module Run.Reader
  ( Reader(..)
  , READER
  , _reader
  , liftReader
  , liftReaderAt
  , ask
  , asks
  , askAt
  , asksAt
  , local
  , localAt
  , runReader
  , runReaderAt
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Run (Run)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

newtype Reader e a = Reader (e -> a)

derive newtype instance functorReader :: Functor (Reader e)

type READER e r = (reader :: Reader e | r)

_reader :: Proxy "reader"
_reader = Proxy

liftReader :: forall e a r. Reader e a -> Run (READER e + r) a
liftReader = liftReaderAt _reader

liftReaderAt
  :: forall t e a r s
   . IsSymbol s
  => Row.Cons s (Reader e) t r
  => Proxy s
  -> Reader e a
  -> Run r a
liftReaderAt = Run.lift

ask :: forall e r. Run (READER e + r) e
ask = askAt _reader

askAt
  :: forall t e r s
   . IsSymbol s
  => Row.Cons s (Reader e) t r
  => Proxy s
  -> Run r e
askAt sym = asksAt sym identity

asks :: forall e r a. (e -> a) -> Run (READER e + r) a
asks = asksAt _reader

asksAt
  :: forall t e r s a
   . IsSymbol s
  => Row.Cons s (Reader e) t r
  => Proxy s
  -> (e -> a)
  -> Run r a
asksAt sym f = liftReaderAt sym (Reader f)

local :: forall e a r. (e -> e) -> Run (READER e + r) a -> Run (READER e + r) a
local = localAt _reader

localAt
  :: forall t e a r s
   . IsSymbol s
  => Row.Cons s (Reader e) t r
  => Proxy s
  -> (e -> e)
  -> Run r a
  -> Run r a
localAt sym = \f r -> map f (askAt sym) >>= flip runLocal r
  where
  handle = Run.on sym Left Right
  runLocal = loop
    where
    loop e r = case Run.peel r of
      Left a -> case handle a of
        Left (Reader k) ->
          loop e (k e)
        Right _ ->
          Run.send a >>= runLocal e
      Right a ->
        pure a

runReader :: forall e a r. e -> Run (READER e + r) a -> Run r a
runReader = runReaderAt _reader

runReaderAt
  :: forall t e a r s
   . IsSymbol s
  => Row.Cons s (Reader e) t r
  => Proxy s
  -> e
  -> Run r a
  -> Run t a
runReaderAt sym = loop
  where
  handle = Run.on sym Left Right
  loop e r = case Run.peel r of
    Left a -> case handle a of
      Left (Reader k) ->
        loop e (k e)
      Right a' ->
        Run.send a' >>= runReaderAt sym e
    Right a ->
      pure a
