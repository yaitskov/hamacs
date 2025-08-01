{-# LANGUAGE OverloadedStrings #-}
module Emacs.Api.Native.Package where

import Emacs.Type
    ( EmacsValue,
      Symbol(Symbol),
      FromEmacsValue(fromEv),
      EmacsSymbol(EmacsSymbol),
      MonadEmacs )
import Emacs.Type.FromEmacsValueInstances ()
import Emacs.Type.ToEmacsListInstances ()
import Emacs.Type.ToEmacsSymbolInstances ()
import Emacs.Prelude ( ($), (=<<), void, Text )
import Emacs.Internal.Function ( funcall1 )
import Emacs.Internal.Intern ( intern )

require :: MonadEmacs m => EmacsSymbol -> m ()
require (EmacsSymbol feature) = void $ funcall1 "require" feature

symbolValue :: MonadEmacs m => EmacsSymbol -> m EmacsValue
symbolValue symName = funcall1 "symbol-value" symName

loadPath :: MonadEmacs m => m [Text]
loadPath = fromEv =<< symbolValue =<< intern "load-path"

provide :: MonadEmacs m => Text -> m EmacsValue
provide feature = funcall1 "provide" (Symbol feature)
