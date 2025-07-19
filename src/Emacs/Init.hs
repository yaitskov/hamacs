{-# LANGUAGE OverloadedStrings #-}

module Emacs.Init where

import Control.Exception (AssertionFailed (..))
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Data.Map.Strict qualified as M
import Emacs
import Emacs.Hint
import Emacs.Internal ()
import Emacs.Prelude

import Data.Time.Clock.System
-- import Data.Time.Format

import Foreign.C.Types
import Language.Haskell.Interpreter qualified as HI
import Language.Haskell.Interpreter.Unsafe qualified as HI
import UnliftIO.Environment
import System.FilePath
import System.IO.Unsafe ( unsafePerformIO )
import UnliftIO.Concurrent ( forkIO, modifyMVar_)
import UnliftIO.Directory
import UnliftIO.STM ( readTQueue, writeTQueue, atomically, newTQueueIO, TQueue )
import UnliftIO.Exception


foreign export ccall "emacs_module_init" emacsModuleInit :: EmacsModule


packageDatabase :: MonadIO m => m [GhcDbPath]
packageDatabase =
  lookupEnv "NIX_GHC_LIBDIR"
  <&> fmap (GhcDbPath . (</> "package.conf.d")) . maybeToList
  >>= filterM (doesDirectoryExist . unGhcDbPath)

runHintOn :: TQueue HintReq -> EmacsM ()
runHintOn q = catchAny go oops
  where
    oops (SomeException se) = putStrLn $ "\nTotal OOPS " <> show se  <> "\n"
    go = do
      putStrLn "Package thread is alive"
      HI.unsafeRunInterpreterWithArgs
        [ "-no-user-package-db" -- , "-v"
        , "-package-env", "-"
        , "-package-db", "/home/dan/pro/haskell/my/hamacs/dist-newstyle/packagedb/ghc-9.12.2"
        , "-package", "base"
        , "-package", "hamacs"
        , "-package", "exceptions"
        , "-package", "filepath"
        , "-package", "cases"
        , "-package", "containers"
        , "-package", "mtl"
        , "-package", "hint"
        , "-package", "relude"
        , "-package", "stm"
        , "-package", "text"
        , "-package", "unliftio"
        , "-package", "unliftio-core"
        , "-XGHC2024" -- LambdaCase
        ] (do
          HI.set [ HI.languageExtensions HI.:= [HI.NoImplicitPrelude] ]
          -- HI.loadModules [  "MyHint" ]  -- "MyModule" ]
          HI.setImports [ "UnliftIO.STM", "Emacs.Type", "Emacs.Hint", "Relude" ]
          putStrLn $ "before unsafeInterpret"
          r :: StateT (TQueue HintReq) EmacsM () <- HI.unsafeInterpret "runHintQueue" "StateT (TQueue HintReq) EmacsM ()"
          lift (evalStateT r q)

          -- () <- HI.runStmt codeAsStr -- (HI.as :: EmacsM ())
          -- r :: () <- HI.interpret codeAsStr (HI.as :: ())
          -- r :: EmacsM () <- HI.unsafeInterpret codeAsStr "EmacsM ()"
          -- putStrLn  "Eval Finished "
          -- lift r
        ) >>=
       \case
         Left e -> do
           putStrLn $ "Hint failed " <> show e
           -- pure 111
         Right () -> do
           putStrLn $ "Hint succes "



hintWorker :: {-(MonadMask m, MonadUnliftIO m) => -} TQueue HintReq -> EmacsM () -- (Either HI.InterpreterError ())
hintWorker q = catchAny go oops
  where
    oops (SomeException se) = putStrLn $ "\nTotal OOPS " <> show se  <> "\n"
    go = do
      putStrLn "Hint worker is alive"
      HI.unsafeRunInterpreterWithArgs
        [ "-no-user-package-db" -- , "-v"
        , "-package-env", "-"
        , "-package-db", "/home/dan/pro/haskell/my/hamacs/dist-newstyle/packagedb/ghc-9.12.2"
        , "-package", "base"
        , "-package", "hamacs"
        , "-package", "exceptions"
        , "-package", "filepath"
        , "-package", "cases"
        , "-package", "containers"
        , "-package", "mtl"
        , "-package", "hint"
        , "-package", "relude"
        , "-package", "stm"
        , "-package", "text"
        , "-package", "unliftio"
        , "-package", "unliftio-core"
        ] (do
          HI.set [ HI.languageExtensions HI.:= [HI.NoImplicitPrelude] ]
          HI.loadModules [  "MyModule" ]
          HI.setImports [ "Emacs.Type", "MyModule", "Relude" ]
          doTheQueue
          -- () <- HI.runStmt codeAsStr -- (HI.as :: EmacsM ())
          -- r :: () <- HI.interpret codeAsStr (HI.as :: ())
          -- r :: EmacsM () <- HI.unsafeInterpret codeAsStr "EmacsM ()"
          -- putStrLn  "Eval Finished "
          -- lift r
        ) >>=
       \case
         Left e -> do
           putStrLn $ "Hint failed " <> show e
           -- pure 111
         Right () -> do
           putStrLn $ "Hint succes "
           -- pure 222
  -- HI.runInterpreter doTheQueue >>= \case
  --   Left e -> putStrLn $ "Hint failed " <> show e
  --   Right r -> putStrLn $ "Hint succes " <> show r

    doTheQueue = do
      atomically (readTQueue q) >>= \case
        PutMVarOnReady m -> putMVar m ()
        PingHint -> putStrLn "Hint Worker is still alive" >> doTheQueue
        SyncPing _t _m -> error "Sync ping is not supporte" >> doTheQueue
        KillHint -> putStrLn "Dead fish"
        -- afh <- accessFormHint <$> lift getPState
        -- putStrLn $ "Inside inter Eval [" <> codeAsStr <> "] accessFormHint = " <> toString afh

        EvalHsCode code ->
          let codeAsStr = toString code in do
            putStrLn $ "Eval inside Hint [" <> codeAsStr <> "]"
            r :: EmacsM () <- HI.unsafeInterpret codeAsStr "EmacsM ()"
            lift r
            doTheQueue
            -- HI.setImports [ "Prelude" ]
            -- r <- HI.interpret codeAsStr (HI.as :: IO ())
            -- putStrLn $ "Eval Finished " <> show r


initHint :: {-(MonadMask m, MonadUnliftIO m) => -} EmacsM Hint
initHint = do
  q <- newTQueueIO
  -- ctx <- ask
  Hint q <$> forkIO (hintWorker q) -- runReaderT ctx (hintWorker q))
    -- (\case
    --     Right _ -> putStrLn "Hint Worker stopped successfully"
    --     Left e -> die $ "Hint got exception " <> displayException e)

hamacsPackageQueues :: MVar (Map Text (TQueue HintReq))
hamacsPackageQueues = unsafePerformIO (newMVar mempty)

emacsModuleInit :: EmacsModule
emacsModuleInit = defmodule "mymodule" $ do
  Hint q _hintid <- initHint
  defun "mysquare" $ \i -> do
    message "haskell squre function called"
    return (i*i :: Int)
  defun "myplus" $ \ x y -> do
    message "haskell plus function called"
    return (x + y :: Int)
  defun "hint-how-are-you" (ping q) -- $ \ (_ :: Int) -> do
  defun "eval-in-calling-thread" evalSync
  defun "ping-hamacs-package" pingPackage
  defun "load-hamacs-package" loadHamacsPackage
  defun "eval-haskell" $ \hsCode -> do
    message "eval-haskell"
    message $ "Eval [" <> hsCode <> "]"
    atomically . writeTQueue q $ EvalHsCode hsCode
    return (777 :: Int)
  where
    pingPackage :: Text -> EmacsM ()
    pingPackage pkgName = do
      pm <- readMVar hamacsPackageQueues
      case M.lookup pkgName pm of
        Nothing -> throwIO . AssertionFailed $ "Hamacs package [" <> show pkgName <> "] is not found"
        Just pkgQueue -> do
          em <- newEmptyMVar
          beforeCall <- liftIO (systemSeconds <$> getSystemTime)
          atomically . writeTQueue pkgQueue $ SyncPing pkgName em
          takeMVar em
          afterRespond <- liftIO (systemSeconds <$> getSystemTime)
          putTextLn $ "Duration: " <> show (afterRespond - beforeCall) <> " seconds"


    loadHamacsPackage :: Text -> EmacsM Text
    loadHamacsPackage moduleName = do
      pkgQueue <- newTQueueIO
      responseMvar <- newEmptyMVar
      atomically . writeTQueue pkgQueue $ PutMVarOnReady responseMvar
      tid <- forkIO $ runHintOn pkgQueue

      putStr $ "Pending module " <> show moduleName <> " from tread " <> show tid <> " while loading is complete..."
      takeMVar responseMvar
      putStrLn $ "   " <> show moduleName <> " module loading is complete"

      modifyMVar_ hamacsPackageQueues (pure . M.insert moduleName pkgQueue)
      pure $ "package [" <> moduleName <> "] is ready"


    evalSync :: Text -> EmacsM Int
    evalSync hsCode = do
      -- mapM_ (setEnv "GHC_PACKAGE_PATH" . (<> ":/home/dan/pro/haskell/my/hamacs/dist-newstyle/build/x86_64-linux/ghc-9.4.7/hamacs-0.0.1") . unGhcDbPath) =<< packageDatabase
      HI.unsafeRunInterpreterWithArgs
        [ "-no-user-package-db" -- , "-v"
        , "-package-env", "-"
        , "-package-db", "/home/dan/pro/haskell/my/hamacs/dist-newstyle/packagedb/ghc-9.12.2"
        , "-package", "base"
        , "-package", "hamacs"
        , "-package", "exceptions"
        , "-package", "filepath"
        , "-package", "cases"
        , "-package", "containers"
        , "-package", "mtl"
        , "-package", "hint"
        , "-package", "relude"
        , "-package", "stm"
        , "-package", "text"
        , "-package", "unliftio"
        , "-package", "unliftio-core"
        ] (
        let codeAsStr = toString hsCode in do
          afh <- accessFormHint <$> lift getPState
          putStrLn $ "Inside inter Eval [" <> codeAsStr <> "] accessFormHint = " <> toString afh
          HI.set [ HI.languageExtensions HI.:= [HI.NoImplicitPrelude] ]
          HI.loadModules [  "MyModule" ]
          HI.setImports [ "Emacs.Type", "MyModule", "Relude" ]
          -- () <- HI.runStmt codeAsStr -- (HI.as :: EmacsM ())
          -- r :: () <- HI.interpret codeAsStr (HI.as :: ())
          r :: EmacsM () <- HI.unsafeInterpret codeAsStr "EmacsM ()"
          putStrLn  "Eval Finished "
          lift r
        ) >>=
       \case
         Left e -> do
           putStrLn $ "Hint failed " <> show e
           pure 111
         Right () -> do
           putStrLn $ "Hint succes "
           pure 222
    ping :: TQueue HintReq -> EmacsM Int
    ping q = do
      message "Before putting Ping to Queue"
      atomically $ writeTQueue q PingHint
      pure 111
