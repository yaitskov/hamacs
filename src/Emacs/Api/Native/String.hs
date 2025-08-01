{-# LANGUAGE OverloadedStrings #-}
module Emacs.Api.Native.String where

import Emacs.Internal.Function ( funcall1, funcall2, car )
-- import Emacs.Internal.List
import Emacs.Prelude ( ($), (=<<), void, Text )
import Emacs.Type
    ( ToEmacsValue, EmacsValue, FromEmacsValue(fromEv), MonadEmacs )
import Emacs.Type.FromEmacsValueInstances ()
import Emacs.Type.ToEmacsListInstances ()
import Emacs.Type.ToEmacsSymbolInstances ()
import Emacs.Type.ToEmacsValueInstances ()

format :: (MonadEmacs m, ToEmacsValue a) => Text -> [a] -> m Text
format fmt args = fromEv =<< funcall2 "format" fmt args

message :: MonadEmacs m => Text -> m Text
message t = fromEv =<< funcall1 "message" t

print :: (MonadEmacs m, ToEmacsValue v) => v -> m ()
print ev = void $ funcall1 "print" ev

evalString :: MonadEmacs m => Text -> m EmacsValue
evalString t =
  funcall1 "eval" =<< (car =<< funcall1 "read-from-string" t)
