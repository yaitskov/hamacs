{-# LANGUAGE OverloadedStrings #-}
module Emacs.Internal (
    module Emacs.Type,
    nonLocalExitThrow,
    mkGlobalRef,
    initCtx,
    getEnv,
    runEmacsM,
    getEmacsEnvFromRT,
    -- Type relaties one..
    typeOf,
    isTypeOf,
    -- emacs -> haskell
    extractInteger,
    extractString,
    --
    eq,
    isNotNil,
    isNil,
    -- mk
    mkFunction,
    mkInteger,
    mkString,
    intern,
    intern',
    mkList,
    mkNil,
    mkT,
    --
    funcall,
    errorHandle,
    checkExitStatus
    ) where

import Control.Exception (catch, throwIO)
import Data.Text.Foreign qualified as TF
import Emacs.Type
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.StablePtr
import Foreign.Storable
import GHC.Ptr
import qualified Data.List as List
import Relude
import Unsafe.Coerce ( unsafeCoerce )

initCtx :: MonadIO m => EmacsEnv -> m Ctx
initCtx env = pure env

getEnv :: (Monad m, HasEmacsCtx m) => m EmacsEnv
getEnv = getEmacsCtx

-- Logging here is not a good idea. When passing high order function,
-- which could be invoked manytimes, its get quite slow.
runEmacsM :: MonadIO m => Ctx -> EmacsM a -> m a
runEmacsM ctx action =
  liftIO $ runReaderT action ctx

foreign import ccall _get_emacs_env_from_rt
  :: Ptr ()
  -> IO EmacsEnv

getEmacsEnvFromRT :: Ptr () -> IO EmacsEnv
getEmacsEnvFromRT =
  _get_emacs_env_from_rt

foreign import ccall _type_of
  :: EmacsEnv
  -> EmacsValue
  -> IO EmacsValue

typeOf :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> m EmacsType
typeOf ev = do
  env <- getEnv
  typeP <- checkExitStatus $ liftIO (_type_of env ev)
  types <- forM emacsTypes $ \t -> do
             q <- intern (emacsTypeSymbolName t)
             b <- eq q typeP
             return (b, t)
  case List.find fst types of
    Just (_, t) -> return t
    Nothing     -> error "no type"

isTypeOf :: (MonadIO m, HasEmacsCtx m) => EmacsType -> EmacsValue -> m Bool
isTypeOf ty ev = do
  t <- typeOf ev
  return $ t == ty

-- 引数が integer じゃない場合多分 signal が投げられる
foreign import ccall _extract_integer
  :: EmacsEnv
  -> EmacsValue
  -> IO CIntMax

extractInteger :: (MonadIO m, HasEmacsCtx m, Num b) => EmacsValue -> m b
extractInteger ev = do
  env <- getEnv
  i <- checkExitStatus $ liftIO (_extract_integer env ev)
  return (fromIntegral i)

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

-- toS = id -- toString

extractString :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> m Text
extractString ev = do
  env <- getEnv
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

-- eq は bool 返すのだが、haskell では CBool は用意していないので int
-- にして返している。module_eq は珍しく MODULE_FUNCTION_BEGIN を使って
-- いない。
foreign import ccall _eq
  :: EmacsEnv
  -> EmacsValue
  -> EmacsValue
  -> IO CInt

eq :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> EmacsValue -> m Bool
eq ev0 ev1 = do
  env <- getEnv
  r <- liftIO $ _eq env ev0 ev1
  return (r == 1)

foreign import ccall _is_not_nil
  :: EmacsEnv
  -> EmacsValue
  -> IO CInt

isNotNil :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> m Bool
isNotNil ev = do
  env <- getEnv
  r <- liftIO $ _is_not_nil env ev
  return (r == 1)

isNil :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> m Bool
isNil = (fmap . fmap) not isNotNil

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
-- unsafePerformIO $ do
--   np <- newStablePtr 0
--   pure $ coerce np

mkFunction :: (MonadIO m, HasEmacsCtx m) =>
  ([EmacsValue] -> EmacsM EmacsValue) -> Int -> Int -> Text -> m EmacsValue
mkFunction f minArity' maxArity' doc' = do
  let minArity = fromIntegral minArity' :: CPtrdiff
      maxArity = fromIntegral maxArity' :: CPtrdiff
  -- datap <- getPStateStablePtr
  -- rt <- rtPtr <$> getEmacsCtx -- :: m Ctx
  stubp <- liftIO (wrapEFunctionStub stub)
  env <- getEnv
  checkExitStatus $ liftIO (TF.withCString doc' $ \doc ->
    _make_function env minArity maxArity stubp doc stableNullPtr)
  where
    stub :: EFunctionStub
    stub env nargs args _pstatep = do
      -- _env <- getEmacsEnvFromRT rt
      errorHandle env $ do
        -- pstate <- deRefStablePtr pstatep
        es <- fmap EmacsValue <$> peekArray (fromIntegral nargs) args
        runEmacsM env (f es)

errorHandle :: EmacsEnv -> IO EmacsValue -> IO EmacsValue
errorHandle env action =
  action `catch` emacsExceptionHandler
         `catch` haskellExceptionHandler
  where
    haskellExceptionHandler :: SomeException -> IO EmacsValue
    haskellExceptionHandler e = do
      ctx <- initCtx env
      runEmacsM ctx $ do
        funcallExit <- nonLocalExitCheck
        nil <- mkNil
        when (funcallExit == EmacsFuncallExitReturn) $ do
          mes <- mkString (toText $ displayException e)
          arg <- mkList [mes]
          sym <- intern "haskell-error"
          nonLocalExitSignal sym arg --
        return nil

    emacsExceptionHandler :: EmacsException -> IO EmacsValue
    emacsExceptionHandler e@(EmacsException funcallExit a0 a1) = do
      let setter = case funcallExit of
                     EmacsFuncallExitSignal -> _non_local_exit_signal
                     EmacsFuncallExitThrow -> _non_local_exit_throw
                     _ -> error $ "Enexpected " <> show e
      setter env a0 a1
      return a0

checkExitStatus :: (MonadIO m, HasEmacsCtx m) => m a -> m a
checkExitStatus action = do
  v <- action
  funcallExit <- nonLocalExitCheck
  when (funcallExit /= EmacsFuncallExitReturn) $ do
    (_,a0,a1) <- nonLocalExitGet
    nonLocalExitClear
    liftIO . throwIO $ EmacsException funcallExit a0 a1
  return v

foreign import ccall _make_integer
  :: EmacsEnv
  -> CIntMax
  -> IO EmacsValue

mkInteger :: (Show n, MonadIO m, HasEmacsCtx m, Integral n) => n -> m EmacsValue
mkInteger i' = do
  let i = fromIntegral i' :: CIntMax
  env <- getEnv
  checkExitStatus (liftIO $ _make_integer env i)

-- Create emacs symbol
foreign import ccall _make_string
  :: EmacsEnv
  -> CString
  -> CPtrdiff
  -> IO EmacsValue

mkString :: (MonadIO m, HasEmacsCtx m) => Text -> m EmacsValue
mkString str = do
  env <- getEnv
  checkExitStatus $ liftIO (TF.withCStringLen str $ \(cstr,len) ->
                               _make_string env cstr (fromIntegral len))

-- Symbol
-- https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Symbols.html
foreign import ccall _intern
  :: EmacsEnv
  -> CString
  -> IO EmacsValue

intern' :: (MonadIO m, HasEmacsCtx m) => Text -> m EmacsSymbol
intern' = pure . EmacsSymbol <=< intern

intern :: (MonadIO m, HasEmacsCtx m) => Text -> m EmacsValue
intern str = create
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
      env <- getEnv
      checkExitStatus (liftIO (TF.withCString str $ \cstr -> _intern env cstr))

mkNil :: (MonadIO m, HasEmacsCtx m) => m EmacsValue
mkNil = do
  q0 <- intern "symbol-value"
  q1 <- intern "nil"
  funcall q0 [q1]

mkT :: (MonadIO m, HasEmacsCtx m) => m EmacsValue
mkT = do
  q0 <- intern "symbol-value"
  q1 <- intern "t"
  funcall q0 [q1]

mkList :: (MonadIO m, HasEmacsCtx m) => [EmacsValue] -> m EmacsValue
mkList evs = do
  listQ <- intern "list"
  funcall listQ evs

foreign import ccall _make_global_ref
  :: EmacsEnv
  -> EmacsValue
  -> IO GlobalEmacsValue

mkGlobalRef :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> m GlobalEmacsValue
mkGlobalRef ev = do
  env <- getEnv
  checkExitStatus $ liftIO (_make_global_ref env ev)

foreign import ccall _non_local_exit_check
 :: EmacsEnv
 -> IO CInt

nonLocalExitCheck :: (MonadIO m, HasEmacsCtx m) => m EmacsFuncallExit
nonLocalExitCheck = do
  env <- getEnv
  toEnum . fromIntegral <$> liftIO (_non_local_exit_check env)

foreign import ccall _non_local_exit_signal
  :: EmacsEnv
  -> EmacsValue
  -> EmacsValue
  -> IO ()

nonLocalExitSignal :: EmacsValue -> EmacsValue -> EmacsM ()
nonLocalExitSignal sym val = do
  env <- getEnv
  liftIO $ _non_local_exit_signal env sym val

foreign import ccall _non_local_exit_throw
  :: EmacsEnv
  -> EmacsValue
  -> EmacsValue
  -> IO ()

nonLocalExitThrow :: EmacsValue -> EmacsValue -> EmacsM ()
nonLocalExitThrow sym val = do
  env <- getEnv
  liftIO $ _non_local_exit_throw env sym val

foreign import ccall _non_local_exit_clear
  :: EmacsEnv
  -> IO ()

nonLocalExitClear :: (MonadIO m, HasEmacsCtx m) => m ()
nonLocalExitClear = do
  env <- getEnv
  liftIO $ _non_local_exit_clear env

foreign import ccall _non_local_exit_get
  :: EmacsEnv
  -> Ptr EmacsValue
  -> Ptr EmacsValue
  -> IO CInt

nonLocalExitGet :: (MonadIO m, HasEmacsCtx m) => m (EmacsFuncallExit,EmacsValue,EmacsValue)
nonLocalExitGet = do
  env <- getEnv
  liftIO $ do
    a0' <- malloc
    a1' <- malloc
    fe  <- _non_local_exit_get env a0' a1'
    a0  <- peek a0'
    a1  <- peek a1'
    free a0'
    free a1'
    return (toEnum (fromIntegral fe), a0, a1)

foreign import ccall _funcall
  :: EmacsEnv
  -> EmacsValue
  -> CPtrdiff
  -> Ptr EmacsValue
  -> IO EmacsValue

funcall :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> [EmacsValue] -> m EmacsValue
funcall func args = do
  env <- getEnv
  checkExitStatus $ liftIO (withArray args $ \carr -> _funcall env func argsLen carr)
  where
    argsLen = fromIntegral (length args) :: CPtrdiff
