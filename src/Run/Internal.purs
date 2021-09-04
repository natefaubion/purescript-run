module Run.Internal
  ( Choose(..)
  , CHOOSE
  , _choose
  , toRows
  , fromRows
  ) where

import Prelude

import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data Choose a
  = Empty
  | Alt (Boolean -> a)

derive instance functorChoose :: Functor Choose

type CHOOSE r = (choose :: Choose | r)

_choose :: Proxy "choose"
_choose = Proxy

toRows
  :: forall f r1 r2 a
   . TypeEquals (Proxy r1) (Proxy r2)
  => f r1 a
  -> f r2 a
toRows = unsafeCoerce

fromRows
  :: forall f r1 r2 a
   . TypeEquals (Proxy r1) (Proxy r2)
  => f r2 a
  -> f r1 a
fromRows = unsafeCoerce
