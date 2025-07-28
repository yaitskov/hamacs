{-# OPTIONS_GHC -fno-warn-orphans #-}
module Emacs.Type.ToEmacsValueInstances where

import Emacs.Type
import Emacs.Prelude
import Emacs.Internal.String
import Emacs.Internal.Integer
import Emacs.Internal.Nil
import Emacs.Internal.T

-- import Emacs.Type.ToEmacsSymbolInstances ()

instance ToEmacsValue EmacsValue where
  toEv = pure

-- Integer
instance ToEmacsValue Int where
  toEv = mkInteger

-- String
instance ToEmacsValue Text where
  toEv = mkString

-- Symbol
instance ToEmacsValue EmacsSymbol where
  toEv = pure . asEmacsValue


-- Kwyword
instance ToEmacsValue EmacsKeyword where
  toEv = pure . asEmacsValue

-- Bool
instance ToEmacsValue Bool where
  toEv True  = mkT
  toEv False = mkNil

-- Nil
instance ToEmacsValue () where
  toEv _ = mkNil

-- List
instance ToEmacsValue EmacsList where
  toEv = pure . asEmacsValue

-- Cons
instance ToEmacsValue EmacsCons where
  toEv = pure . asEmacsValue


-- Function
-- Can only handle function with no arguments.
-- Use mkFunctionFromCallable for no args.
instance ToEmacsValue EmacsFunction where
  toEv = pure . asEmacsValue
