{-# LANGUAGE OverloadedStrings #-}
module Emacs.Api.Native.Buffer where

import Emacs.Type
import Emacs.Type.CallableInstances ()
import Emacs.Type.FromEmacsValueInstances ()
import Emacs.Type.ToEmacsListInstances ()
import Emacs.Type.ToEmacsSymbolInstances ()
import Emacs.Prelude ( Monad((>>=)), Int )
import Emacs.Internal.Function ( funcall0 )
import Emacs.Internal.Intern ( intern )

point :: MonadEmacs m => m Int
point = funcall0 "point" >>= fromEv

mark :: MonadEmacs m => m Int
mark = funcall0 "mark" >>= fromEv

saveExcursion :: (MonadEmacs m, ToEmacsValue a, FromEmacsValue a) => [m a] -> m a
saveExcursion as = do
  se <- intern "save-excursion"
  callOverEmacs se as
