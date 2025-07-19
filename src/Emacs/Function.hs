{-# LANGUAGE OverloadedStrings #-}

module Emacs.Function where

import Relude
import Emacs.Core

setFunction :: Text -> EmacsValue -> EmacsM ()
setFunction name f = do
  void $ funcall2 "fset" (Symbol name) f

defun' :: Text -> Doc -> Arity -> ([EmacsValue] -> EmacsM EmacsValue) -> EmacsM ()
defun' name (Doc doc) (Arity a) f =
  setFunction name =<< mkFunction f a a doc

defun :: Callable f => Text -> f -> EmacsM ()
defun name f =
  setFunction name =<< mkFunctionFromCallable f
