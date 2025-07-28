module Emacs.Internal.Integer where

import Emacs.Type ( HasEmacsCtx(..), EmacsValue(..), EmacsEnv(..) )
import Emacs.Prelude
import Emacs.Internal.Check ( checkExitStatus )
import Foreign.C.Types ( CIntMax(..) )


foreign import ccall _make_integer
  :: EmacsEnv
  -> CIntMax
  -> IO EmacsValue

mkInteger :: (Show n, MonadIO m, HasEmacsCtx m, Integral n) => n -> m EmacsValue
mkInteger i' = do
  let i = fromIntegral i' :: CIntMax
  env <- getEmacsCtx
  checkExitStatus (liftIO $ _make_integer env i)

-- 引数が integer じゃない場合多分 signal が投げられる
foreign import ccall _extract_integer
  :: EmacsEnv
  -> EmacsValue
  -> IO CIntMax

extractInteger :: (MonadIO m, HasEmacsCtx m, Num b) => EmacsValue -> m b
extractInteger ev = do
  env <- getEmacsCtx
  i <- checkExitStatus $ liftIO (_extract_integer env ev)
  return (fromIntegral i)
