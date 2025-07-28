{-# OPTIONS_GHC -fno-warn-orphans #-}
module Emacs.Type.ToEmacsFunctionInstances where

import Emacs.Internal.Function
import Emacs.Prelude
import Emacs.Type
import Emacs.Type.CallableInstances ()


instance (FromEmacsValue a, Callable b) => ToEmacsValue (a -> b) where
  toEv = (asEmacsValue<$>) . toEmacsFunction

instance ToEmacsFunction EmacsFunction where
  toEmacsFunction = pure

instance (FromEmacsValue a, Callable b) => ToEmacsFunction (a -> b) where
  toEmacsFunction f = EmacsFunction <$> mkFunctionFromCallable f
