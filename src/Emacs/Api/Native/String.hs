{-# LANGUAGE OverloadedStrings #-}
module Emacs.Api.Native.String where

import Emacs.Type
import Emacs.Prelude
import Emacs.Core

format :: ToEmacsValue a => Text -> [a] -> EmacsM Text
format fmt args = fromEv =<< funcall2 "format" fmt args
