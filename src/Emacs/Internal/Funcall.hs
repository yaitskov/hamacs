module Emacs.Internal.Funcall where

import Emacs.Type
    ( HasEmacsCtx(..), EmacsValue(..), EmacsSymbol(..), EmacsEnv(..) )
import Emacs.Prelude
    ( ($), fromIntegral, Foldable(length), IO, MonadIO(..) )
import Emacs.Internal.Check ( checkExitStatus )
import Foreign.C.Types ( CPtrdiff(..) )
import Foreign.Marshal.Array ( withArray )
import GHC.Ptr ( Ptr )

foreign import ccall _funcall
  :: EmacsEnv
  -> EmacsValue
  -> CPtrdiff
  -> Ptr EmacsValue
  -> IO EmacsValue

funcall :: (MonadIO m, HasEmacsCtx m) => EmacsSymbol -> [EmacsValue] -> m EmacsValue
funcall (EmacsSymbol func) args = do
  env <- getEmacsCtx
  checkExitStatus $ liftIO (withArray args $ \carr -> _funcall env func argsLen carr)
  where
    argsLen = fromIntegral (length args) :: CPtrdiff
