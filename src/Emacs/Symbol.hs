{-# LANGUAGE OverloadedStrings #-}
module Emacs.Symbol where

import Emacs.Prelude
import Emacs.Type
import Emacs.Type.ToEmacsValueInstances ()
import Emacs.Type.CallableInstances ()
import Emacs.Type.ToEmacsSymbolInstances ()
import Emacs.Internal.Function ( funcall1, funcall2, funcall3 )
import Emacs.Internal.Nil ( isNotNil )
import Emacs.Internal.String ( extractString )

allSymbols :: MonadEmacs m => m [EmacsValue]
allSymbols = do
  ref <- liftIO $ newIORef []
  _ <- funcall1 "mapatoms" (accum ref)
  liftIO $ readIORef ref
  where
    accum :: IORef [EmacsValue] -> EmacsValue -> IO ()
    accum ref sym = modifyIORef ref (sym:)

-- Symbol has four slots.
--
--  1. name
--  2. value (* can have buffer local value)
--  3. function
--  4. property list (* can have buffer local list)

getSymbolName :: MonadEmacs m => Text -> m Text
getSymbolName name =
  extractString =<< funcall1 "symbol-name" (Symbol name)

setValue :: (MonadEmacs m, ToEmacsValue a) => Text -> a -> m EmacsValue
setValue name val =
  funcall2 "set" (Symbol name) val

-- Could throw exception if the symbol is not setted.
getValue :: MonadEmacs m => Text -> m EmacsValue
getValue name =
  funcall1 "symbol-value" (Symbol name)

-- if Symbol exists (included in obarray) and a value is bounded.
isBounded :: MonadEmacs m => Text -> m  Bool
isBounded name =
  isNotNil =<< funcall1 "boundp" (Symbol name)

-- Buffer local
--
-- If the variable is buffer local, you need to use
-- getDefaultValue/setDefaultValue to set/get the global variable.
getDefaultValue :: MonadEmacs m => Text -> m EmacsValue
getDefaultValue name =
  funcall1 "default-value" (Symbol name)

setDefaultValue :: (MonadEmacs m, ToEmacsValue a) => Text -> a -> m EmacsValue
setDefaultValue name val =
  funcall2 "set-default" (Symbol name) val

symbolProperty :: MonadEmacs m => Text -> Text -> m (Maybe EmacsValue)
symbolProperty name property = do
  ev <- funcall2 "get" (Symbol name) (Symbol property)
  b <- isNotNil ev
  return $ if b then Just ev else Nothing

setSymbolProperty
  :: (MonadEmacs m, ToEmacsValue v)
  => Text
  -> Text
  -> v
  -> m EmacsValue
setSymbolProperty name property value =
  funcall3 "put" (Symbol name) (Symbol property) value
