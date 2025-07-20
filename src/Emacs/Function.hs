{-# LANGUAGE OverloadedStrings #-}

module Emacs.Function where

import Relude
import Emacs.Core

setFunction :: (MonadIO m, HasEmacsCtx m) => Text -> EmacsValue -> m ()
setFunction name f = do
  void $ funcall2 "fset" (Symbol name) f

defun' :: Text -> EmDoc -> Arity -> ([EmacsValue] -> EmacsM EmacsValue) -> EmacsM ()
defun' name (EmDoc doc) (Arity a) f =
  setFunction name =<< mkFunction f a a doc

defun :: Callable f => Text -> f -> EmacsM ()
defun name f =
  setFunction name =<< mkFunctionFromCallable f
