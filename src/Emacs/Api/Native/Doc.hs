module Emacs.Api.Native.Doc where

import Emacs.Internal.Function ( funcall1 )
import Emacs.Prelude ( Monad((>>=)), Text )
import Emacs.Type
    ( MonadEmacs, FromEmacsValue(fromEv), EmacsSymbol )

-- | return True if symbol is an interactive function
documentation :: MonadEmacs m => EmacsSymbol -> m Text
documentation funName = funcall1 "documentation" funName >>= fromEv
