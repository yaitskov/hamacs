{-# OPTIONS_GHC -fno-warn-orphans #-}
module Emacs.Type.ToEmacsConsInstances where

import Emacs.Type
import Emacs.Prelude
import Emacs.Internal.Function

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
