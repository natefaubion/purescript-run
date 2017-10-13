module Run.Maybe where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Variant (FProxy, SProxy(..))
import Data.Maybe (Maybe(..))
import Run (Run, lift, on, send)
import Run as Run


data MaybeEffect a
  = JustEffect a
  | NothingEffect
derive instance maybeEffectInstance :: Functor MaybeEffect

type MAYBE = FProxy MaybeEffect

_maybe :: SProxy "maybe"
_maybe = SProxy

liftMaybe :: forall a r. Maybe a -> Run(maybe :: MAYBE | r) a
liftMaybe (Just a) = lift _maybe (JustEffect a)
liftMaybe Nothing = lift _maybe NothingEffect

handleMaybe :: forall a r. MaybeEffect a -> (Run r (Maybe a))
handleMaybe (JustEffect a) = pure $ Just a
handleMaybe NothingEffect  = pure Nothing 

-- runPure :: forall r1 r2 a. (VariantF r1 (Run r1 a) -> Step (Run r1 a) (VariantF r2 (Run r1 a))) -> Run r1 a -> Run r2 a
runMaybe :: forall a r. Run (maybe :: MAYBE | r) a -> Run r (Maybe a)
runMaybe = loop
  where
    handle = on _maybe Left Right
    loop r = case Run.peel r of
      Left a -> case handle a of
        Left NothingEffect -> pure Nothing
        Left (JustEffect b) -> loop b
        Right a' -> send a' >>= runMaybe
      Right a -> pure $ Just a
