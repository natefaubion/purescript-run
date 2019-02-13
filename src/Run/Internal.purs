module Run.Internal
  ( Choose(..)
  , CHOOSE
  , _choose
  , Empty(..)
  , EMPTY
  , _empty
  , toRows
  , fromRows
  ) where

import Prelude

import Data.Functor.Variant (FProxy, SProxy(..))
import Type.Equality (class TypeEquals)
import Type.Row (RProxy)
import Unsafe.Coerce (unsafeCoerce)

data Choose a
  = Alt (Boolean → a)

derive instance functorChoose ∷ Functor Choose

type CHOOSE = FProxy Choose

_choose ∷ SProxy "choose"
_choose = SProxy

data Empty a = Empty

derive instance functorEmpty :: Functor Empty

type EMPTY = FProxy Empty

_empty :: SProxy "empty"
_empty = SProxy

toRows
  ∷ ∀ f r1 r2 a
  . TypeEquals (RProxy r1) (RProxy r2)
  ⇒ f r1 a
  → f r2 a
toRows = unsafeCoerce

fromRows
  ∷ ∀ f r1 r2 a
  . TypeEquals (RProxy r1) (RProxy r2)
  ⇒ f r2 a
  → f r1 a
fromRows = unsafeCoerce
