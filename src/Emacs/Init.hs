{-# LANGUAGE OverloadedStrings #-}

module Emacs.Init where

import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Emacs
import Emacs.Internal ()
import Emacs.Prelude
import Foreign.C.Types
import Language.Haskell.Interpreter qualified as HI
import Language.Haskell.Interpreter.Unsafe qualified as HI
import UnliftIO.Environment
import System.FilePath
import UnliftIO.Concurrent
import UnliftIO.Directory
import UnliftIO.STM
-- import Unsafe.Coerce

foreign export ccall "emacs_module_init" emacsModuleInit :: EmacsModule

data HintReq = EvalHsCode Text | PingHint | KillHint deriving (Show, Eq)

data Hint
  = Hint
    { hintQueue :: TQueue HintReq
    , hintThreadId :: ThreadId
    }

newtype GhcDbPath = GhcDbPath { unGhcDbPath :: FilePath }  deriving (Show, Eq)

packageDatabase :: MonadIO m => m [GhcDbPath]
packageDatabase =
  lookupEnv "NIX_GHC_LIBDIR"
  <&> fmap (GhcDbPath . (</> "package.conf.d")) . maybeToList
  >>= filterM (doesDirectoryExist . unGhcDbPath)

hintWorker :: (MonadMask m, MonadIO m) => TQueue HintReq -> m () -- (Either HI.InterpreterError ())
hintWorker q = do
  putStrLn "Hint worker is alive"
  doTheQueue
  -- HI.runInterpreter doTheQueue >>= \case
  --   Left e -> putStrLn $ "Hint failed " <> show e
  --   Right r -> putStrLn $ "Hint succes " <> show r
  where
    doTheQueue = do
      atomically (readTQueue q) >>= \case
        PingHint -> putStrLn "Hint Worker is still alive" >> doTheQueue
        KillHint -> putStrLn "Dead fish"
        EvalHsCode code ->
          let codeAsStr = toString code in do
            putStrLn $ "Eval [" <> codeAsStr <> "]"
            -- HI.setImports [ "Prelude" ]
            -- r <- HI.interpret codeAsStr (HI.as :: IO ())
            -- putStrLn $ "Eval Finished " <> show r
            doTheQueue

initHint :: (MonadMask m, MonadUnliftIO m) => m Hint
initHint = do
  q <- newTQueueIO
  Hint q <$> forkIO (hintWorker q)
    -- (\case
    --     Right _ -> putStrLn "Hint Worker stopped successfully"
    --     Left e -> die $ "Hint got exception " <> displayException e)

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
  defun "eval-haskell" $ \hsCode -> do
    message "eval-haskell"
    message $ "Eval [" <> hsCode <> "]"
    atomically . writeTQueue q $ EvalHsCode hsCode
    return (777 :: Int)
  where
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
          HI.setImports [ "Emacs.Type", "MyModule" ]
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
