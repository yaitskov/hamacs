module Emacs.Api.Native.Eval where

import Emacs.Internal.Function ( funcall1 )
import Emacs.Prelude ( Monad((>>=)), Bool )
import Emacs.Type
    ( MonadEmacs, FromEmacsValue(fromEv), EmacsSymbol )

-- | return True if symbol is an interactive function
commandp :: MonadEmacs m => EmacsSymbol -> m Bool
commandp funName = funcall1 "commandp" funName >>= fromEv
