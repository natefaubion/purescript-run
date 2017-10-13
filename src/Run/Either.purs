module Run.Either
( liftEither
, left
, right
, runEither
, _either
, EITHER  
) where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Variant (FProxy, SProxy(..))
import Run (Run, lift, on, send)
import Run as Run

type EITHER e = FProxy (Either e)

_either :: SProxy "either"
_either = SProxy

liftEither :: forall e a r. Either e a -> Run(either :: EITHER e | r) a
liftEither = lift _either

right :: forall e a r. a -> Run(either :: EITHER e | r) a
right = (lift _either) <<< Right

left :: forall e a r. e -> Run(either :: EITHER e | r) a
left = (lift _either) <<< Left

runEither :: forall e a r. Run (either :: EITHER e | r) a -> Run r (Either e a)
runEither = loop
  where
  handle = on _either Left Right
  loop r = case Run.peel r of
    Left a -> case handle a of
      Left (Left e)   -> pure $ Left e 
      Left (Right a') -> loop a'
      Right a'        -> send a' >>= runEither
    Right a -> pure $ Right a
