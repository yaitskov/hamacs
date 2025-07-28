module Emacs.Internal.FuncallN where

import Emacs.Internal.Funcall ( funcall )
import Emacs.Internal.Intern ( intern )
import Emacs.Prelude
import Emacs.Type
    ( EmacsM, HasEmacsCtx, EmacsValue, ToEmacsValue(..) )

funcall1
  :: (MonadIO m, HasEmacsCtx m, ToEmacsValue a)
  => Text
  -> a
  -> m EmacsValue
funcall1 fname ev0 =
  join $ funcall <$> intern fname
                 <*> sequence [toEv ev0]

funcall2
  :: (MonadIO m, HasEmacsCtx m, ToEmacsValue a, ToEmacsValue b)
  => Text
  -> a
  -> b
  -> m EmacsValue
funcall2 fname ev0 ev1 =
  join $ funcall <$> intern fname
                 <*> sequence [toEv ev0, toEv ev1]

funcall3
  :: (ToEmacsValue a, ToEmacsValue b, ToEmacsValue c)
  => Text
  -> a
  -> b
  -> c
  -> EmacsM EmacsValue
funcall3 fname ev0 ev1 ev2 =
  join $ funcall <$> intern fname
                 <*> sequence [toEv ev0, toEv ev1, toEv ev2]
