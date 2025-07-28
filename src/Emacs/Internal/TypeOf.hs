{-# LANGUAGE OverloadedStrings #-}
module Emacs.Internal.TypeOf where

import Emacs.Internal.Check
import Emacs.Internal.Eq
import Emacs.Internal.Intern
import Emacs.Prelude
import Emacs.Type
import Emacs.Type.ToEmacsValueInstances ()

foreign import ccall _type_of
  :: EmacsEnv
  -> EmacsValue
  -> IO EmacsValue

typeOf :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> m EmacsType
typeOf ev = do
  env <- getEmacsCtx
  typeP <- checkExitStatus $ liftIO (_type_of env ev)
  types <- forM emacsTypes $ \t -> do
             EmacsSymbol q <- intern (emacsTypeSymbolName t)
             b <- eq q typeP
             return (b, t)
  case find fst types of
    Just (_, t) -> return t
    Nothing     -> error "no type"

isTypeOf :: (MonadIO m, HasEmacsCtx m) => EmacsType -> EmacsValue -> m Bool
isTypeOf ty ev = do
  t <- typeOf ev
  return $ t == ty
