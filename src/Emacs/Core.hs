{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Emacs.Core
  ( module X
  , defmodule
  , ToEmacsValue(..)
  , FromEmacsValue(..)
  , ToEmacsSymbol(..)
  , ToEmacsFunction(..)
  , NativeCallable(..)
  , evalString
  , provide
  , message
  , print
  ) where

import Emacs.Internal as X
import Emacs.Prelude hiding (print)
import Emacs.Api.Native.Package ( provide )
import Emacs.Api.Native.String ( message, print, evalString )

defmodule :: Text -> NativeEmacsM a -> EmacsModule
defmodule name mod' ert = do
  env <- getEmacsEnvFromRT ert
  _ <- errorHandle env $ runNativeEmacsM env (mod' >> provide name)
  return 0
