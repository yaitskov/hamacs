{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Emacs.Type.ToEmacsKeywordInstances where

import Emacs.Internal.Intern
import Emacs.Prelude
import Emacs.Type
import Emacs.Type.ToEmacsValueInstances ()

instance ToEmacsValue Keyword where
  toEv = (asEmacsValue<$>) . toEmacsKeyword
instance ToEmacsKeyword EmacsKeyword where
  toEmacsKeyword = pure
instance ToEmacsKeyword Keyword where
  toEmacsKeyword (Keyword t) = EmacsKeyword . unEmacsSymbol <$> intern (":" <> t)
