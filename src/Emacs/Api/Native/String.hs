{-# LANGUAGE OverloadedStrings #-}
module Emacs.Api.Native.String where

import Emacs.Internal.FuncallN ( funcall1, funcall2 )
import Emacs.Internal.List
import Emacs.Prelude ( ($), (=<<), void, Text )
import Emacs.Type
    ( EmacsValue, ToEmacsValue, EmacsM, FromEmacsValue(fromEv) )
import Emacs.Type.FromEmacsValueInstances ()
import Emacs.Type.ToEmacsListInstances ()
import Emacs.Type.ToEmacsSymbolInstances ()
import Emacs.Type.ToEmacsValueInstances ()

format :: ToEmacsValue a => Text -> [a] -> EmacsM Text
format fmt args = fromEv =<< funcall2 "format" fmt args

message :: Text -> EmacsM ()
message t = void $ funcall1 "message" t

print :: ToEmacsValue v => v -> EmacsM ()
print ev = void $ funcall1 "print" ev

evalString :: Text -> EmacsM EmacsValue
evalString t =
  funcall1 "eval" =<< (car =<< funcall1 "read-from-string" t)
