{-# OPTIONS_GHC -fno-warn-orphans #-}
module Emacs.Type.ToEmacsSymbolInstances where

import Emacs.Internal.Intern ( intern )
import Emacs.Prelude ( Applicative(pure), (<$>) )
import Emacs.Type
import Emacs.Type.ToEmacsValueInstances ()

instance ToEmacsValue Symbol where
  toEv x = do
    unEmacsSymbol <$> toEmacsSymbol x
instance ToEmacsSymbol EmacsSymbol where
  toEmacsSymbol = pure
instance ToEmacsSymbol Symbol      where
  toEmacsSymbol (Symbol t) = intern t
