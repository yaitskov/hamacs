{-# LANGUAGE OverloadedStrings #-}
module Emacs.Hint where


-- import Control.Concurrent.STM.TQueue (  )
import Data.Text.Foreign qualified as TF
import Emacs.Core
import Emacs.Prelude
-- import Emacs.Type
import Emacs.Function
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
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
      -- CPtrdiff
      (MVar (Either SomeException EmacsValue))
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
  getEmacsCtx = EmacsHintM (emacsCtxM <$> ask)

runHintQueue :: EmacsHintM ()
runHintQueue = do
  q <- asks packageInQueue
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
    CallFun fn fnArgs responseVar -> do
      catchAny
        (do
            c :: Ctx <- asks emacsCtxM
            !r <- liftIO (runReaderT (fn fnArgs) c)
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
  q <- asks packageInQueue

  stubp <- liftIO (wrapHintFunctionStub (stub q))
  env <- emacsEnv . emacsCtxM <$> ask -- getEnv

  checkExitStatus $ liftIO (TF.withCString doc' $ \emFunDoc ->
    _make_function env minArity maxArity stubp emFunDoc unitStablePtr)
  where
    stub :: TQueue HintReq -> HintFunctionStub
    stub q env nargs args _pstatep =
      errorHandle env $ do
        es <- fmap EmacsValue <$> peekArray (fromIntegral nargs) args
        respMvar <- newEmptyMVar
        atomically $ writeTQueue q (CallFun f es respMvar)
        readMVar respMvar >>= \case
          Left se -> throwIO se
          Right r -> pure r

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
