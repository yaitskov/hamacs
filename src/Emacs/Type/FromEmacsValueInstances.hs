{-# OPTIONS_GHC -fno-warn-orphans #-}
module Emacs.Type.FromEmacsValueInstances where

import Emacs.Internal.Integer ( extractInteger )
import Emacs.Internal.Nil ( isNotNil )
import Emacs.Internal.String ( extractString )
import Emacs.Prelude ( Applicative(pure), Int, Text, Bool, void, (.) )
import Emacs.Type ( FromEmacsValue(..), EmacsValue, EmacsFunction(..) )

instance FromEmacsValue Bool where
  fromEv = isNotNil

instance FromEmacsValue Int where
  fromEv = extractInteger

instance FromEmacsValue Text where
  fromEv = extractString

instance FromEmacsValue () where
  fromEv = void . pure

instance FromEmacsValue EmacsValue where
  fromEv = pure

instance FromEmacsValue EmacsFunction where
  fromEv = pure . EmacsFunction
