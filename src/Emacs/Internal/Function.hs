{-# LANGUAGE OverloadedStrings #-}
module Emacs.Internal.Function where

import Data.Text.Foreign qualified as TF
import Emacs.Internal.Check
import Emacs.Internal.Intern
import Emacs.Internal.String
import Emacs.Internal.List
import Emacs.Internal.Nil
import Emacs.Prelude
import Emacs.Type
-- import Emacs.Type.FromEmacsValueInstances ()
-- import Emacs.Type.ToEmacsValueInstances ()
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.StablePtr
import GHC.Ptr
import Unsafe.Coerce ( unsafeCoerce )

-- TODO: arity と doc は Arity と Doc 型にするべきかな。
foreign import ccall _make_function
  :: EmacsEnv
  -> CPtrdiff
  -> CPtrdiff
  -> FunPtr EFunctionStub
  -> CString
  -> StablePtr a
  -> IO EmacsValue

-- TODO: ??? これ StablePtr の効果も兼ねている？
foreign import ccall "wrapper" wrapEFunctionStub
  :: EFunctionStub
  -> IO (FunPtr EFunctionStub)

stableNullPtr :: StablePtr Void
stableNullPtr = unsafeCoerce nullPtr


mkFunction :: (MonadIO m, HasEmacsCtx m) =>
  ([EmacsValue] -> EmacsM EmacsValue) -> Int -> Int -> Text -> m EmacsValue
mkFunction f minArity' maxArity' doc' = do
  let minArity = fromIntegral minArity' :: CPtrdiff
      maxArity = fromIntegral maxArity' :: CPtrdiff
  stubp <- liftIO (wrapEFunctionStub stub)
  env <- getEmacsCtx
  checkExitStatus $ liftIO (TF.withCString doc' $ \emDoc ->
    _make_function env minArity maxArity stubp emDoc stableNullPtr)
  where
    stub :: EFunctionStub
    stub env nargs args _pstatep = do
      errorHandle env $ do
        es <- fmap EmacsValue <$> peekArray (fromIntegral nargs) args
        runEmacsM env (f es)

errorHandle :: EmacsEnv -> IO EmacsValue -> IO EmacsValue
errorHandle env action =
  action `catch` emacsExceptionHandler
         `catch` haskellExceptionHandler
  where
    haskellExceptionHandler :: SomeException -> IO EmacsValue
    haskellExceptionHandler e = do
      ctx <- pure env
      runEmacsM ctx $ do
        funcallExit <- nonLocalExitCheck
        nil <- mkNil
        when (funcallExit == EmacsFuncallExitReturn) $ do
          mes <- mkString (toText $ displayException e)
          arg <- mkList [mes]
          sym <- intern "haskell-error"
          nonLocalExitSignal sym arg
        return nil

    emacsExceptionHandler :: EmacsException -> IO EmacsValue
    emacsExceptionHandler e@(EmacsException funcallExit a0 a1) = do
      let setter = case funcallExit of
                     EmacsFuncallExitSignal -> _non_local_exit_signal
                     EmacsFuncallExitThrow -> _non_local_exit_throw
                     _ -> error $ "Enexpected " <> show e
      setter env a0 a1
      return a0

mkFunctionFromCallable :: (MonadIO m, HasEmacsCtx m) => Callable f => f -> m EmacsValue
mkFunctionFromCallable f = do
  let a = arity f
  mkFunction func a a ""
  where
    func :: [EmacsValue] -> EmacsM EmacsValue
    func es = do
      res <- call f es
      case res of
        Right ev -> return ev
        Left e -> fail $ "mkFunctionFromCallable failed for f with " <> show (length es) <> " args: " <> show e
