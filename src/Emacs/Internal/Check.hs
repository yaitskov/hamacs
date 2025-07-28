module Emacs.Internal.Check where

import Control.Exception (throwIO)
import Emacs.Type
import Foreign.C.Types ( CInt(..) )
import Foreign.Marshal.Alloc ( free, malloc )
import Foreign.Storable ( Storable(peek) )
import GHC.Ptr ( Ptr )
import Relude



checkExitStatus :: (MonadIO m, HasEmacsCtx m) => m a -> m a
checkExitStatus action = do
  v <- action
  funcallExit <- nonLocalExitCheck
  when (funcallExit /= EmacsFuncallExitReturn) $ do
    (_,a0,a1) <- nonLocalExitGet
    nonLocalExitClear
    liftIO . throwIO $ EmacsException funcallExit a0 a1
  return v

foreign import ccall _non_local_exit_check
 :: EmacsEnv
 -> IO CInt

nonLocalExitCheck :: (MonadIO m, HasEmacsCtx m) => m EmacsFuncallExit
nonLocalExitCheck = do
  env <- getEmacsCtx
  toEnum . fromIntegral <$> liftIO (_non_local_exit_check env)

foreign import ccall _non_local_exit_signal
  :: EmacsEnv
  -> EmacsValue
  -> EmacsValue
  -> IO ()

nonLocalExitSignal :: EmacsSymbol -> EmacsValue -> EmacsM ()
nonLocalExitSignal (EmacsSymbol sym) val = do
  env <- getEmacsCtx
  liftIO $ _non_local_exit_signal env sym val

foreign import ccall _non_local_exit_throw
  :: EmacsEnv
  -> EmacsValue
  -> EmacsValue
  -> IO ()

nonLocalExitThrow :: EmacsValue -> EmacsValue -> EmacsM ()
nonLocalExitThrow sym val = do
  env <- getEmacsCtx
  liftIO $ _non_local_exit_throw env sym val

foreign import ccall _non_local_exit_clear
  :: EmacsEnv
  -> IO ()

nonLocalExitClear :: (MonadIO m, HasEmacsCtx m) => m ()
nonLocalExitClear = do
  env <- getEmacsCtx
  liftIO $ _non_local_exit_clear env

foreign import ccall _non_local_exit_get
  :: EmacsEnv
  -> Ptr EmacsValue
  -> Ptr EmacsValue
  -> IO CInt

nonLocalExitGet ::
  (MonadIO m, HasEmacsCtx m) =>
  m (EmacsFuncallExit, EmacsValue, EmacsValue)
nonLocalExitGet = do
  env <- getEmacsCtx
  liftIO $ do
    a0' <- malloc
    a1' <- malloc
    fe  <- _non_local_exit_get env a0' a1'
    a0  <- peek a0'
    a1  <- peek a1'
    free a0'
    free a1'
    return (toEnum (fromIntegral fe), a0, a1)
