{-# LANGUAGE OverloadedStrings #-}
module Emacs.Internal.T where

import Emacs.Type
    ( EmacsValue,
      ToEmacsValue(..),
      HasEmacsCtx,
      EmacsSymbol(EmacsSymbol) )
import Emacs.Prelude ( Eq, Show, MonadIO )
import Emacs.Internal.Intern ( intern )
import Emacs.Internal.Funcall ( funcall )

data T = T deriving (Show, Eq)

instance ToEmacsValue T where
  toEv !_ = mkT

mkT :: (MonadIO m, HasEmacsCtx m) => m EmacsValue
mkT = do
  q0 <- intern "symbol-value"
  EmacsSymbol q1 <- intern "t"
  funcall q0 [q1]
