{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Emacs.Core
  ( module Emacs.Internal
  , defmodule
  , mkCons
  , ToEmacsValue(..)
  , FromEmacsValue(..)
  , ToEmacsSymbol(..)
  , ToEmacsFunction(..)
  , funcall1, funcall2, funcall3
  , mkFunctionFromCallable
  , extractList
  , NativeCallable(..)
  , car
  , cdr
  , evalString
  , provide
  , message
  , print
  ) where

import Emacs.Internal
-- import Emacs.Internal.FuncallN ( funcall1, funcall2, funcall3 )
-- import Emacs.Internal.List ( mkCons, car, cdr, extractList )
import Emacs.Internal.Function
    ( mkFunctionFromCallable,
      funcall1,
      funcall2,
      funcall3,
      mkCons,
      car,
      cdr,
      extractList )
import Emacs.Prelude hiding (print)
import Emacs.Api.Native.Package ( provide )
import Emacs.Api.Native.String ( message, print, evalString )

defmodule :: Text -> NativeEmacsM a -> EmacsModule
defmodule name mod' ert = do
  env <- getEmacsEnvFromRT ert
  _ <- errorHandle env $ runNativeEmacsM env (mod' >> provide name)
  return 0
