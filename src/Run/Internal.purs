module Run.Internal
  ( Choose(..)
  , CHOOSE
  , _choose
  , toRows
  , fromRows
  ) where

import Prelude

import Data.Functor.Variant (FProxy, SProxy(..))
import Type.Data.Row (RProxy)
import Type.Equality (class TypeEquals)
import Unsafe.Coerce (unsafeCoerce)

data Choose a
  = Empty
  | Alt (Boolean → a)

derive instance functorChoose ∷ Functor Choose

type CHOOSE = FProxy Choose

_choose ∷ SProxy "choose"
_choose = SProxy

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
