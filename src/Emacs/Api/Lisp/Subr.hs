{-# LANGUAGE OverloadedStrings #-}
module Emacs.Api.Lisp.Subr where

import Emacs.Type
import Emacs.Prelude
import Emacs.Core

addToList :: (MonadEmacs m, ToEmacsValue a) => EmacsSymbol -> a -> m ()
addToList listVar element = void $ funcall2 "add-to-list" listVar element
