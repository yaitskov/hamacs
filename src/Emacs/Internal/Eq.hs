module Emacs.Internal.Eq where

import Emacs.Prelude
import Emacs.Type
import Foreign.C.Types ( CInt(..) )

-- eq は bool 返すのだが、haskell では CBool は用意していないので int
-- にして返している。module_eq は珍しく MODULE_FUNCTION_BEGIN を使って
-- いない。
foreign import ccall _eq
  :: EmacsEnv
  -> EmacsValue
  -> EmacsValue
  -> IO CInt

eq :: (MonadEmacs m, ToEmacsValue a) => a -> a -> m Bool
eq ev0' ev1' = do
  env <- getEmacsCtx
  ev0 <- toEv ev0'
  ev1 <- toEv ev1'
  r <- liftIO $ _eq env ev0 ev1
  return (r == 1)
