{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Emacs.Hint where


import Data.Text.Foreign qualified as TF
import Emacs.Core
import Emacs.Prelude
import Emacs.Function
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
-- import Foreign.Storable
import Foreign.StablePtr
import System.IO.Unsafe
import UnliftIO.Concurrent (ThreadId)
import UnliftIO.Exception ( catchAny, throwIO )
import UnliftIO.STM ( TQueue, writeTQueue, readTQueue, atomically)
import Foreign.Marshal.Array
-- import Foreign.Marshal.Alloc

unitStablePtr :: StablePtr ()
unitStablePtr = unsafePerformIO (newStablePtr ())

type HintFunctionStub
  = EmacsEnv
  -> CPtrdiff
  -> Ptr (Ptr ())
  -> StablePtr ()
  -> IO EmacsValue

foreign import ccall "wrapper" wrapHintFunctionStub
  :: HintFunctionStub
  -> IO (FunPtr HintFunctionStub)

data HintReq
  = EvalHsCode Text
  | PingHint
  | SyncPing Text (MVar ())
  | PutMVarOnReady (MVar ())
  | CallFun
      ([EmacsValue] -> EmacsM EmacsValue) -- (FunPtr EFunctionStub)
      [EmacsValue] --  (Ptr (Ptr ()))
      (MVar (Either SomeException EmacsValue))
      EmacsEnv
  | KillHint

data Hint
  = Hint
    { hintQueue :: TQueue HintReq
    , hintThreadId :: ThreadId
    }

newtype GhcDbPath = GhcDbPath { unGhcDbPath :: FilePath }  deriving (Show, Eq)

data EmacsHintConf
  = EmacsHintConf
  { packageName :: Text
  , packageInQueue :: TQueue HintReq
  , emacsCtxM :: Ctx
  }

newtype EmacsHintM a = EmacsHintM (ReaderT EmacsHintConf IO a)
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadUnliftIO)

instance MonadReader EmacsHintConf EmacsHintM where
   ask = EmacsHintM ask
   local f (EmacsHintM m) = EmacsHintM (local f m)

instance HasEmacsCtx EmacsHintM where
  getEmacsCtx = EmacsHintM (asks (.emacsCtxM))

data HintQueueWorkerConf
  = HintQueueWorkerConf
  { packageName :: Text
  , packageInQueue :: TQueue HintReq
  }

runHintQueue :: ReaderT HintQueueWorkerConf IO ()
runHintQueue = do
  q <- asks (.packageInQueue)
  atomically (readTQueue q) >>= \case
    PutMVarOnReady m -> do
      putStrLn "Before put () to hint MVAR"
      putMVar m ()
      runHintQueue
    PingHint -> putStrLn "Hint Worker is still alive" >> runHintQueue
    SyncPing msg m -> do
      putStrLn $ "Sync Ping [" <> show msg <> "]"
      putMVar m ()
      runHintQueue
    KillHint -> putStrLn "Dead fish"
    CallFun fn fnArgs responseVar ee -> do
      catchAny
        (do !r <- runEmacsM ee (fn fnArgs)
            putMVar responseVar $! Right r
        )
        (putMVar responseVar . Left)
      runHintQueue
    EvalHsCode code ->
      let codeAsStr = toString code in do
        putStrLn $ "Eval inside Hint [" <> codeAsStr <> "]"
        -- r :: EmacsM () <- HI.unsafeInterpret codeAsStr "EmacsM ()"
        -- lift r
        -- runHintQueue

foreign import ccall _make_function
  :: EmacsEnv
  -> CPtrdiff
  -> CPtrdiff
  -> FunPtr HintFunctionStub
  -> CString
  -> StablePtr ()
  -> IO EmacsValue

mkHintFunction :: ([EmacsValue] -> EmacsM EmacsValue) -> Int -> Int -> Text -> EmacsHintM EmacsValue
mkHintFunction f minArity' maxArity' doc' = do
  let minArity = fromIntegral minArity' :: CPtrdiff
      maxArity = fromIntegral maxArity' :: CPtrdiff
  -- datap <- newSta getPStateStablePtr
  -- get and bind queue
  q <- asks (.packageInQueue)
  -- rt <- rtPtr <$> getEmacsCtx
  env <- getEmacsCtx
  stubp <- liftIO (wrapHintFunctionStub (stub env q))
  checkExitStatus $ liftIO (TF.withCString doc' $ \emFunDoc ->
    _make_function env minArity maxArity stubp emFunDoc unitStablePtr)
  where
    stub :: EmacsEnv -> TQueue HintReq -> HintFunctionStub
    stub _env q env nargs args _pstatep = do
      -- fArg <- peek args
      -- hi <- _extract_integer env (EmacsValue fArg)
      -- putStrLn $ " env " <> show env <> "; nargs: " <> show nargs <> "; input = " <> show hi
      -- p <- _make_integer env 33
      -- putStrLn $ " p(env) " <> show p


      -- env <- getEmacsEnvFromRT rt
      errorHandle env $ do
        es <- fmap EmacsValue <$> peekArray (fromIntegral nargs) args
        respMvar <- newEmptyMVar
        atomically $ writeTQueue q (CallFun f es respMvar env)
        -- putStrLn $ "Before readding mvar" <> show env
        readMVar respMvar >>= \case
          Left se -> do
            -- putStrLn "Before throw exception"
            throwIO se
          Right r -> do
            -- hr <- _extract_integer env r
            -- putStrLn $ "Before return to Emacs " <> show r <> " ; hr = " <> show hr
            pure r

mkHintFunctionFromCallable :: Callable f => f -> EmacsHintM EmacsValue
mkHintFunctionFromCallable f = do
  let a = arity f
  mkHintFunction func a a "Emacs Symbol Docs"
  where
    func :: [EmacsValue] -> EmacsM EmacsValue
    func es = do
      res <- call f es
      case res of
        Right ev -> return ev
        Left e -> fail $ "mkHintFunctionFromCallable failed for f with " <> show (length es) <> " args: " <> show e

defunHint :: Callable f => Text -> f -> EmacsHintM ()
defunHint name f =
  setFunction name =<< mkHintFunctionFromCallable f
