module Emacs.Api.Native.Eval where

import Emacs.Internal.Function ( funcall1 )
import Emacs.Prelude ( Monad((>>=)), Bool )
import Emacs.Type
    ( MonadEmacs, FromEmacsValue(fromEv), EmacsSymbol )
-- import Emacs.Type.CallableInstances ()
-- import Emacs.Type.FromEmacsValueInstances ()
-- import Emacs.Type.ToEmacsSymbolInstances ()

commandp :: MonadEmacs m => EmacsSymbol -> m Bool
commandp funName = funcall1 "commandp" funName >>= fromEv
