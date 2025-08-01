{-# LANGUAGE OverloadedStrings #-}
module Emacs.Api.Native.Buffer where

import Emacs.Type

import Emacs.Type.CallableInstances ()
import Emacs.Type.FromEmacsValueInstances ()
import Emacs.Type.ToEmacsSymbolInstances ()
import Emacs.Prelude
import Emacs.Internal.Function
import Emacs.Internal.Nil
import Emacs.Internal.Intern ( intern )

newtype BufPos = BufPos { unBufPos :: Int } deriving newtype (Show, Eq, Ord, Num, ToEmacsValue)

instance FromEmacsValue BufPos where
  fromEv  = BufPos <.> fromEv

point :: MonadEmacs m => m BufPos
point = funcall0 "point" >>= fromEv

mark :: MonadEmacs m => m BufPos
mark = funcall0 "mark" >>= fromEv

gotoChar :: MonadEmacs m => BufPos -> m BufPos
gotoChar p = funcall1 "goto-char" p >>= fromEv

saveExcursion :: (MonadEmacs m, ToEmacsValue a, FromEmacsValue a) => m a -> m a
saveExcursion as = do
  se <- intern "save-excursion"
  callOverEmacs se [as]

beginningOfLine :: MonadEmacs m => m Nil
beginningOfLine = funcall0 "beginning-of-line" >>= fromEv
