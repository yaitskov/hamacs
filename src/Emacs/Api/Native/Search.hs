{-# LANGUAGE OverloadedStrings #-}
module Emacs.Api.Native.Search where

import Emacs.Api.Native.Buffer ( BufPos )
import Emacs.Internal.Function ( funcall1 )
import Emacs.Prelude
import Emacs.Type
import Emacs.Type.CallableInstances ()
import Emacs.Type.FromEmacsValueInstances ()
import Emacs.Type.ToEmacsSymbolInstances ()

newtype EmacsRegexp = EmacsRegexp Text deriving newtype (Show, Eq, ToEmacsValue, IsString)

reSearchForward :: MonadEmacs m => EmacsRegexp -> m BufPos
reSearchForward er =
  funcall1 "re-search-forward" er >>= fromEv
