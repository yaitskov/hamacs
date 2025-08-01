{-# OPTIONS_GHC -fno-warn-orphans #-}
module Emacs.Type.FromEmacsValueInstances where

import Emacs.Prelude
import Emacs.Type
import Emacs.Internal.Integer
import Emacs.Internal.String


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
