{-# LANGUAGE OverloadedStrings #-}
module Emacs.Api.Native.Package where

import Emacs.Type
    ( HasEmacsCtx,
      EmacsValue,
      EmacsSymbol(EmacsSymbol),
      EmacsM,
      Symbol(Symbol),
      FromEmacsValue(fromEv) )
import Emacs.Type.FromEmacsValueInstances ()
import Emacs.Type.ToEmacsListInstances ()
import Emacs.Type.ToEmacsSymbolInstances ()
import Emacs.Prelude ( ($), (=<<), void, MonadIO, Text )
import Emacs.Internal.FuncallN ( funcall1 )
import Emacs.Internal.Intern ( intern )

require :: EmacsSymbol -> EmacsM ()
require (EmacsSymbol feature) = void $ funcall1 "require" feature

symbolValue :: EmacsSymbol -> EmacsM EmacsValue
symbolValue symName = funcall1 "symbol-value" symName

loadPath :: EmacsM [Text]
loadPath = fromEv =<< symbolValue =<< intern "load-path"

provide :: (MonadIO m, HasEmacsCtx m) => Text -> m EmacsValue
provide feature = funcall1 "provide" (Symbol feature)
