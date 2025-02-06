{-# LANGUAGE OverloadedStrings #-}
module Emacs.Init where

import Emacs
import Foreign.C.Types

foreign export ccall "emacs_module_init" emacsModuleInit :: EmacsModule

emacsModuleInit :: EmacsModule
emacsModuleInit = defmodule "mymodule" $ do
  defun "mysquare" $ \i -> do
    message "haskell squre function called"
    return (i*i :: Int)
