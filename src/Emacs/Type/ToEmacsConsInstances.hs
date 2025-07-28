module Emacs.Type.ToEmacsConsIntances where

import Emacs.Type
import Emacs.Prelude

instance (ToEmacsValue a, ToEmacsValue b) => ToEmacsValue (a, b) where
  toEv = (asEmacsValue<$>) . toEmacsCons
-- Cons
instance ToEmacsCons EmacsCons where
  toEmacsCons = pure
instance (ToEmacsValue a, ToEmacsValue b) => ToEmacsCons (a, b) where
  toEmacsCons (a,b) = do
    av <- toEv a
    bv <- toEv b
    mkCons av bv
