{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
module Emacs.Hint where


import Data.Text.Foreign qualified as TF
import Emacs.Core
import Emacs.Function ( setFunction )
import Emacs.Hint.Type
import Emacs.Prelude
import Foreign.C.String ( CString )
import Foreign.C.Types ( CPtrdiff(..) )
import Foreign.Marshal.Array ( peekArray )
import Foreign.Ptr ( FunPtr )
import Foreign.StablePtr ( newStablePtr, StablePtr )
import GHC.Conc (ThreadStatus (ThreadFinished), threadStatus)
import System.IO.Unsafe ( unsafePerformIO )
import UnliftIO.Concurrent (forkIO)
import UnliftIO.Exception ( catchAny )
import UnliftIO.STM (TQueue, writeTQueue, readTQueue, atomically)


unitStablePtr :: StablePtr ()
unitStablePtr = unsafePerformIO (newStablePtr ())

foreign import ccall "wrapper" wrapHintFunctionStub
  :: HintFunctionStub
  -> IO (FunPtr HintFunctionStub)

runHintQueue :: HintQueueRunner
runHintQueue = do
  q <- ask
  atomically (readTQueue q) >>= \case
    PutMVarOnReady m -> do
      putStrLn "Before put () to hint MVAR"
      putMVar m ()
      runHintQueue
    ReThrow e -> throwIO e
    Return v -> pure v
    CallFun fn fnArgs responseVar ee -> do
      catchAny
        (do !r <- liftIO $ runReaderT (fn fnArgs) (Ctx ee q)
            putMVar responseVar $! Right r
        )
        (putMVar responseVar . Left)
      runHintQueue

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
  q <- asks (.hintQueue)
  env <- getEmacsCtx
  stubp <- liftIO (wrapHintFunctionStub (stub env q))
  checkExitStatus $ liftIO (TF.withCString doc' $ \emFunDoc ->
    _make_function env minArity maxArity stubp emFunDoc unitStablePtr)
  where
    stub :: EmacsEnv -> TQueue HintReq -> HintFunctionStub
    stub _env q env nargs args _pstatep = do
      errorHandle env $ do
        es <- fmap EmacsValue <$> peekArray (fromIntegral nargs) args
        respMvar <- newEmptyMVar
        atomically $ writeTQueue q (CallFun f es respMvar env)
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

class CallableArity a => Callable a where
    call :: a -> [EmacsValue] -> EmacsM (Either Text EmacsValue)

instance {-# OVERLAPPING #-} ToEmacsValue a => Callable a where
    call a [] = Right <$> toEv a
    call _ _  = pure $ Left "Too many arguments"

instance {-# OVERLAPPING #-} ToEmacsValue a => Callable (IO a) where
    call a [] = do
      v <- liftIO a
      Right <$> toEv v
    call _ _  = pure $ Left "Too many arguments"

instance {-# OVERLAPPING #-} ToEmacsValue a => CallableArity (EmacsM a) where
  arity _ = 0

instance {-# OVERLAPPING #-} ToEmacsValue a => Callable (EmacsM a) where
  call am [] = do
    a <- am
    Right <$> toEv a
  call _ _  = pure $ Left "Too many arguments"

instance {-# OVERLAPPING #-} (FromEmacsValue a, Callable b) => Callable (a -> b) where
  call f (e:es) = do
    av <- fromEv e
    call (f av) es
  call _ [] = pure $ Left "Too less arguments"


defunHint :: Callable f => Text -> f -> EmacsHintM ()
defunHint name f = do
  sn <- intern name
  setFunction sn =<< mkHintFunctionFromCallable f

instance MonadEmacs EmacsM where
  callOverEmacs (EmacsSymbol s) a = do
    q <- asks (.hintQueue)
    pa <- mkHintFunctionFromCallable a
    tid <- forkIO do
      -- eval would cause another call from Emacs
      catchAny
        (do r <- funcall1 "eval" =<< sequence [pure s, toEv [pa]]
            atomically . writeTQueue q $ Return r
        )
        (\se -> atomically . writeTQueue q $ ReThrow se)
    -- wait queue
    r <- liftIO $ runReaderT runHintQueue q
    tst <- liftIO $ threadStatus tid
    if tst == ThreadFinished
      then fromEv r
      else throwIO . AssertionFailed $ "eval of " <> show s <> " is not finished"
