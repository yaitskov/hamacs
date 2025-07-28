{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Emacs.Internal.List where

import Data.Text qualified as T
import Emacs.Internal.Funcall ( funcall )
import Emacs.Internal.FuncallN ( funcall1, funcall2 )
import Emacs.Internal.Intern ( intern )
import Emacs.Internal.Nil ( isNotNil )
import Emacs.Internal.TypeOf ( isTypeOf )
import Emacs.Prelude
import Emacs.Type
import Emacs.Type.FromEmacsValueInstances ()
import Emacs.Type.ToEmacsValueInstances ()

mkCons
  :: (MonadIO m, HasEmacsCtx m, ToEmacsValue a, ToEmacsValue b)
  => a
  -> b
  -> m EmacsCons
mkCons a b =
  EmacsCons <$> funcall2 "cons" a b

car :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> m EmacsValue
car = funcall1 "car"

cdr :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> m EmacsValue
cdr = funcall1 "cdr"

extractList :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> m [EmacsValue]
extractList ev = do
  isTypeOf ECons ev >>= \case
    False ->
      isTypeOf ENil ev >>= \case
        False ->
          throwIO . AssertionFailed . T.unpack =<< fromEv =<< funcall2 "format" ("Expected a list but got: %s" :: Text) ev
        True -> pure []
    True -> go ev
  where
    go l = do
      nonNil <- isNotNil l
      if nonNil
      then
        (:) <$> (car l) <*> (go =<< cdr l)
      else
        pure []

instance FromEmacsValue a => FromEmacsValue [a] where
  fromEv = mapM fromEv <=< extractList

mkList :: (MonadIO m, HasEmacsCtx m) => [EmacsValue] -> m EmacsValue
mkList evs = do
  listQ <- intern "list"
  funcall listQ evs
