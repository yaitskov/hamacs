module Emacs.Internal.Intern where

import Data.Text.Foreign qualified as TF
import Emacs.Internal.Check ( checkExitStatus )
import Emacs.Type
import Emacs.Prelude
import Foreign.C.String ( CString )

-- Symbol
-- https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Symbols.html
foreign import ccall _intern
  :: EmacsEnv
  -> CString
  -> IO EmacsValue

intern :: (MonadIO m, HasEmacsCtx m) => Text -> m EmacsSymbol
intern = pure . EmacsSymbol <=< intern'

intern' :: (MonadIO m, HasEmacsCtx m) => Text -> m EmacsValue
intern' str = create
  -- s' <- lookupCache
  -- case s' of
  --   Just gev ->
  --     return (castGlobalToEmacsValue gev)
  --   Nothing ->
  --     storeToCache =<< create
  where
  --   lookupCache = do
  --     mapRef <- symbolMap <$> getPState
  --     Map.lookup str <$> (liftIO $ readIORef mapRef)


  --   storeToCache ev = do
  --     mapRef <- symbolMap <$> getPState
  --     gev <- mkGlobalRef ev
  --     liftIO $ modifyIORef mapRef (Map.insert str gev)
  --     return (castGlobalToEmacsValue gev)

    create = do
      env <- getEmacsCtx
      checkExitStatus (liftIO (TF.withCString str $ \cstr -> _intern env cstr))
