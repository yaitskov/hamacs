{-# LANGUAGE OverloadedStrings #-}
module Emacs.Internal.String where

import Data.Text.Foreign qualified as TF
import Emacs.Type
import Emacs.Prelude
import Emacs.Internal.Check ( checkExitStatus )
import Foreign.C.String ( CString )
import Foreign.C.Types ( CInt(..), CPtrdiff(..) )
import Foreign.Marshal.Alloc ( alloca, allocaBytes )
import Foreign.Storable ( Storable(peek) )
import GHC.Ptr ( Ptr, nullPtr )


-- Create emacs symbol
foreign import ccall _make_string
  :: EmacsEnv
  -> CString
  -> CPtrdiff
  -> IO EmacsValue

mkString :: (MonadIO m, HasEmacsCtx m) => Text -> m EmacsValue
mkString str = do
  env <- getEmacsCtx
  checkExitStatus $ liftIO (TF.withCStringLen str $ \(cstr,len) ->
                               _make_string env cstr (fromIntegral len))

-- emacs-module.c 参照
--
--  * Can throw signals(その場合 false が返る)
--  * もし Buffer が null の場合、Length に文字列のutf8で格納する際の
--    必要な長さが設定され、1 を返す
--  * もし Buffer が non-null かつ、Length がutf8を格納するのに足りな
--    い場合、Length に必要な長さが設定され args_out_of_rangeエラーが
--    投げられる。
--  * Bufferが non-null かつ、Length が十分な長さを持っている場合、
--    Buffer に utf8文字列(+最後はnull文字)が格納され、Length には長さ
--    (最後のnull文字を含めたもの)が設定され 1 を返す。
--
foreign import ccall _copy_string_contents
  :: EmacsEnv
  -> EmacsValue
  -> CString         -- Buffer
  -> Ptr CPtrdiff    -- Length
  -> IO CInt

extractString :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> m Text
extractString ev = do
  env <- getEmacsCtx
  checkExitStatus $ liftIO $ alloca $ \length' -> do
    result <- _copy_string_contents env ev nullPtr length'
    if result == 1
      then do
        length'' <- fromIntegral <$> peek length'
        allocaBytes length'' $ \buffer -> do
          _result' <- _copy_string_contents env ev buffer length'
          if result == 1 -- ??
            then TF.peekCString buffer
            else pure ""
      else pure ""
