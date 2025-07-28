{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Emacs.Type.CallableInstances where

import Emacs.Prelude
import Emacs.Type
    ( FromEmacsValue(..), ToEmacsValue(..), Callable(..), EmacsM )


instance {-# OVERLAPPING #-} ToEmacsValue a => Callable a where
    call a [] = Right <$> toEv a
    call _ _  = pure $ Left "Too many arguments"
    arity _ = 0

instance {-# OVERLAPPING #-} ToEmacsValue a => Callable (IO a) where
    call a [] = do
      v <- liftIO a
      Right <$> toEv v
    call _ _  = pure $ Left "Too many arguments"
    arity _ = 0

instance {-# OVERLAPPING #-} ToEmacsValue a => Callable (EmacsM a) where
    call am [] = do
      a <- am
      Right <$> toEv a
    call _ _  = pure $ Left "Too many arguments"
    arity _ = 0

instance {-# OVERLAPPING #-} (FromEmacsValue a, Callable b) => Callable (a -> b) where
  call f (e:es) = do
    av <- fromEv e
    call (f av) es
  call _ [] = pure $ Left "Too less arguments"
  arity f = arity (f (error "Callable (a -> b)")) + 1
