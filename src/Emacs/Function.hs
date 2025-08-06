{-# LANGUAGE OverloadedStrings #-}

module Emacs.Function where

import Relude
import Emacs.Core

setFunction :: MonadEmacs m => EmacsSymbol -> EmacsValue -> m ()
setFunction name f = do
  void $ funcall2 "fset" name f

defun' :: Text -> EmDocString -> Arity -> ([EmacsValue] -> NativeEmacsM EmacsValue) -> NativeEmacsM ()
defun' name doc (Arity a) f = do
  sn <- intern name
  setFunction sn =<< mkFunction f a a doc

defun :: NativeCallable f => Text -> f -> EmDocString -> NativeEmacsM ()
defun name f eds = do
  sn <- intern name
  setFunction sn =<< mkFunctionFromCallable eds f
