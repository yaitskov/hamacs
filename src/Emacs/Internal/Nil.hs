{-# LANGUAGE OverloadedStrings #-}
module Emacs.Internal.Nil where

import Emacs.Internal.Funcall ( funcall )
import Emacs.Internal.Intern ( intern )
import Emacs.Prelude
import Emacs.Type
    ( HasEmacsCtx(..),
      EmacsValue(..),
      EmacsSymbol(EmacsSymbol),
      EmacsEnv(..) )
import Foreign.C.Types ( CInt(..) )

foreign import ccall _is_not_nil
  :: EmacsEnv
  -> EmacsValue
  -> IO CInt

isNotNil :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> m Bool
isNotNil ev = do
  env <- getEmacsCtx
  r <- liftIO $ _is_not_nil env ev
  return (r == 1)

isNil :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> m Bool
isNil = (fmap . fmap) not isNotNil

mkNil :: (MonadIO m, HasEmacsCtx m) => m EmacsValue
mkNil = do
  q0 <- intern "symbol-value"
  EmacsSymbol q1 <- intern "nil"
  funcall q0 [q1]
