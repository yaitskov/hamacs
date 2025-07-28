{-# LANGUAGE OverloadedStrings #-}
module Emacs.Internal.T where

import Emacs.Type
    ( HasEmacsCtx, EmacsValue, EmacsSymbol(EmacsSymbol) )
import Emacs.Prelude ( MonadIO )
import Emacs.Internal.Intern ( intern )
import Emacs.Internal.Funcall ( funcall )

mkT :: (MonadIO m, HasEmacsCtx m) => m EmacsValue
mkT = do
  q0 <- intern "symbol-value"
  EmacsSymbol q1 <- intern "t"
  funcall q0 [q1]
