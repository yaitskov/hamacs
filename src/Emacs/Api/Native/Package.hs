{-# LANGUAGE OverloadedStrings #-}
module Emacs.Api.Native.Package where

import Emacs.Type
import Emacs.Prelude
import Emacs.Core

require :: EmacsSymbol -> EmacsM ()
require (EmacsSymbol feature) = void $ funcall1 "require" feature

symbolValue :: EmacsSymbol -> EmacsM EmacsValue
symbolValue symName = funcall1 "symbol-value" symName

loadPath :: EmacsM [Text]
loadPath = fromEv =<< symbolValue =<< intern' "load-path"
