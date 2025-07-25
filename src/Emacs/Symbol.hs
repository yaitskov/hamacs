{-# LANGUAGE OverloadedStrings #-}
module Emacs.Symbol where

import Relude
import Emacs.Core

allSymbols :: EmacsM [EmacsValue]
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

getSymbolName :: Text -> EmacsM Text
getSymbolName name =
  extractString =<< funcall1 "symbol-name" (Symbol name)

setValue :: ToEmacsValue a => Text -> a -> EmacsM EmacsValue
setValue name val =
  funcall2 "set" (Symbol name) val

-- Could throw exception if the symbol is not setted.
getValue :: Text -> EmacsM EmacsValue
getValue name =
  funcall1 "symbol-value" (Symbol name)

-- if Symbol exists (included in obarray) and a value is bounded.
isBounded :: Text -> EmacsM Bool
isBounded name =
  isNotNil =<< funcall1 "boundp" (Symbol name)

-- Buffer local
--
-- If the variable is buffer local, you need to use
-- getDefaultValue/setDefaultValue to set/get the global variable.
getDefaultValue :: Text -> EmacsM EmacsValue
getDefaultValue name =
  funcall1 "default-value" (Symbol name)

setDefaultValue :: ToEmacsValue a => Text -> a -> EmacsM EmacsValue
setDefaultValue name val =
  funcall2 "set-default" (Symbol name) val

symbolProperty :: Text -> Text -> EmacsM (Maybe EmacsValue)
symbolProperty name property = do
  ev <- funcall2 "get" (Symbol name) (Symbol property)
  b <- isNotNil ev
  return $ if b then Just ev else Nothing

setSymbolProperty
  :: (ToEmacsValue v)
  => Text
  -> Text
  -> v
  -> EmacsM EmacsValue
setSymbolProperty name property value =
  funcall3 "put" (Symbol name) (Symbol property) value
