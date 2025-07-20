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
import Emacs.Text
--import Relude (LText)
import Data.Time.Clock.System
-- import Data.Time.Format

import Foreign.C.Types
import Language.Haskell.Interpreter qualified as HI
import Language.Haskell.Interpreter (ModuleName, MonadInterpreter, ModuleElem (Fun), Id)
import Language.Haskell.Interpreter.Unsafe qualified as HI
import UnliftIO.Environment
import System.FilePath
import System.IO.Unsafe ( unsafePerformIO )
import UnliftIO.Concurrent ( forkIO, modifyMVar_)
import UnliftIO.Directory
import UnliftIO.STM ( writeTQueue, atomically, newTQueueIO, TQueue )
import UnliftIO.Exception


foreign export ccall "emacs_module_init" emacsModuleInit :: EmacsModule


packageDatabase :: MonadIO m => m [GhcDbPath]
packageDatabase =
  lookupEnv "NIX_GHC_LIBDIR"
  <&> fmap (GhcDbPath . (</> "package.conf.d")) . maybeToList
  >>= filterM (doesDirectoryExist . unGhcDbPath)

modElemToFun :: ModuleElem -> Maybe Id
modElemToFun = \case Fun x -> Just x ; _ -> Nothing

modFunctions :: MonadInterpreter m => ModuleName -> m [Id]
modFunctions mm = mapMaybe modElemToFun <$> HI.getModuleExports mm

isEmacsCompatibleFunction :: MonadInterpreter m => Id -> m Bool
isEmacsCompatibleFunction fName =
  HI.typeChecksWithDetails ("mkHintFunctionFromCallable " <> fName) >>= \case
    Left es -> do
      putStrLn $ "Skip " <> fName <> " due: " <> show es
      pure False
    Right ok -> do
      putStrLn $ "Pass " <> fName <> " as " <> ok
      pure True
    -- HI.typeChecks $

runHintOn :: Text -> TQueue HintReq -> EmacsM ()
runHintOn packName q = catchAny go oops
  where
    oops (SomeException se) = putStrLn $ "\nTotal OOPS " <> show se  <> "\n"
    go :: EmacsM ()
    go = do
      let exportingModule = "HaPack"
      putStrLn "Package thread is alive"
      emcCtx <- ask
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
        , "-XOverloadedStrings"
        , "-iscript/" <> toString packName
        ] (do
          HI.set [ HI.languageExtensions HI.:= [HI.NoImplicitPrelude] ]
          HI.loadModules [ exportingModule ]
          HI.setImports [ "UnliftIO.STM", "Emacs.Type", "Emacs.Core", "Emacs.Hint", "Relude"
                        , exportingModule
                        ]
          putStrLn $ "before unsafeInterpret"
          -- list
          fnNames <- modFunctions exportingModule
          printDoc $ hsep ["Functions of",  doc exportingModule, ":"
                            <> linebreak <> tab (vsep fnNames) <> linebreak
                          ]
          emacsCompatibleOnes <- filterM isEmacsCompatibleFunction fnNames
          printDoc $ vsep ["Among them Emacs compatible:", tab (vsep emacsCompatibleOnes)] <> linebreak

          forM_ emacsCompatibleOnes $ \fnName -> do
            let cmd :: Text = "defunHint \"" <> packName <> "-" <> toText (trimX '(' ')' fnName) <> "\" " <> toText fnName
            (EmacsHintM r2) :: EmacsHintM () <- HI.unsafeInterpret (toString cmd)  "EmacsHintM ()"
            liftIO (runReaderT r2 $ EmacsHintConf packName q emcCtx)

          (EmacsHintM r) :: EmacsHintM () <- HI.unsafeInterpret "runHintQueue" "EmacsHintM ()"
          liftIO (runReaderT r $ EmacsHintConf packName q emcCtx)
        ) >>=
       \case
         Left e -> do
           putStrLn $ "Hint failed " <> show e
         Right () -> do
           putStrLn $ "Hint succes "

-- hintWorker :: {-(MonadMask m, MonadUnliftIO m) => -} TQueue HintReq -> EmacsM () -- (Either HI.InterpreterError ())
-- hintWorker q = catchAny go oops
--   where
--     oops (SomeException se) = putStrLn $ "\nTotal OOPS " <> show se  <> "\n"
--     go = do
--       putStrLn "Hint worker is alive"
--       HI.unsafeRunInterpreterWithArgs
--         [ "-no-user-package-db" -- , "-v"
--         , "-package-env", "-"
--         , "-package-db", "/home/dan/pro/haskell/my/hamacs/dist-newstyle/packagedb/ghc-9.12.2"
--         , "-package", "base"
--         , "-package", "hamacs"
--         , "-package", "exceptions"
--         , "-package", "filepath"
--         , "-package", "cases"
--         , "-package", "containers"
--         , "-package", "mtl"
--         , "-package", "hint"
--         , "-package", "relude"
--         , "-package", "stm"
--         , "-package", "text"
--         , "-package", "unliftio"
--         , "-package", "unliftio-core"
--         ] (do
--           HI.set [ HI.languageExtensions HI.:= [HI.NoImplicitPrelude] ]
--           HI.loadModules [  "MyModule" ]
--           HI.setImports [ "Emacs.Type", "MyModule", "Relude" ]
--           doTheQueue
--           -- () <- HI.runStmt codeAsStr -- (HI.as :: EmacsM ())
--           -- r :: () <- HI.interpret codeAsStr (HI.as :: ())
--           -- r :: EmacsM () <- HI.unsafeInterpret codeAsStr "EmacsM ()"
--           -- putStrLn  "Eval Finished "
--           -- lift r
--         ) >>=
--        \case
--          Left e -> do
--            putStrLn $ "Hint failed " <> show e
--            -- pure 111
--          Right () -> do
--            putStrLn $ "Hint succes "
--            -- pure 222
--   -- HI.runInterpreter doTheQueue >>= \case
--   --   Left e -> putStrLn $ "Hint failed " <> show e
--   --   Right r -> putStrLn $ "Hint succes " <> show r

--     doTheQueue = do
--       atomically (readTQueue q) >>= \case
--         PutMVarOnReady m -> putMVar m ()
--         PingHint -> putStrLn "Hint Worker is still alive" >> doTheQueue
--         SyncPing _t _m -> error "Sync ping is not supporte" >> doTheQueue
--         KillHint -> putStrLn "Dead fish"
--         -- afh <- accessFormHint <$> lift getPState
--         -- putStrLn $ "Inside inter Eval [" <> codeAsStr <> "] accessFormHint = " <> toString afh

--         EvalHsCode code ->
--           let codeAsStr = toString code in do
--             putStrLn $ "Eval inside Hint [" <> codeAsStr <> "]"
--             r :: EmacsM () <- HI.unsafeInterpret codeAsStr "EmacsM ()"
--             lift r
--             doTheQueue
--             -- HI.setImports [ "Prelude" ]
--             -- r <- HI.interpret codeAsStr (HI.as :: IO ())
--             -- putStrLn $ "Eval Finished " <> show r


-- initHint :: {-(MonadMask m, MonadUnliftIO m) => -} EmacsM Hint
-- initHint = do
--   q <- newTQueueIO
--   -- ctx <- ask
--   Hint q <$> forkIO (hintWorker q) -- runReaderT ctx (hintWorker q))
--     -- (\case
--     --     Right _ -> putStrLn "Hint Worker stopped successfully"
--     --     Left e -> die $ "Hint got exception " <> displayException e)

hamacsPackageQueues :: MVar (Map Text (TQueue HintReq))
hamacsPackageQueues = unsafePerformIO (newMVar mempty)

emacsModuleInit :: EmacsModule
emacsModuleInit = defmodule "hamacs" $ do
  -- Hint q _hintid <- initHint
  -- defun "mysquare" $ \i -> do
  --   message "haskell squre function called"
  --   return (i*i :: Int)
  -- defun "myplus" $ \ x y -> do
  --   message "haskell plus function called"
  --   return (x + y :: Int)
  -- defun "hint-how-are-you" (ping q) -- $ \ (_ :: Int) -> do
  -- defun "eval-in-calling-thread" evalSync
  defun "hamacs-ping-package" pingPackage
  defun "hamacs-load-package" loadHamacsPackage
  -- defun "eval-haskell" $ \hsCode -> do
  --   message "eval-haskell"
  --   message $ "Eval [" <> hsCode <> "]"
  --   atomically . writeTQueue q $ EvalHsCode hsCode
  --   return (777 :: Int)
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
    loadHamacsPackage hmPgkName = do
      pkgQueue <- newTQueueIO
      responseMvar <- newEmptyMVar
      atomically . writeTQueue pkgQueue $ PutMVarOnReady responseMvar
      tid <- forkIO $ runHintOn hmPgkName pkgQueue

      putStr $ "Pending Hamacs package " <> show hmPgkName <> " from tread " <> show tid <> " while loading is complete..."
      takeMVar responseMvar
      putTextLn $ "  Loading of " <> hmPgkName <> " is complete"

      modifyMVar_ hamacsPackageQueues (pure . M.insert hmPgkName pkgQueue)
      pure $ "package [" <> hmPgkName <> "] is ready"


    -- evalSync :: Text -> EmacsM Int
    -- evalSync hsCode = do
    --   -- mapM_ (setEnv "GHC_PACKAGE_PATH" . (<> ":/home/dan/pro/haskell/my/hamacs/dist-newstyle/build/x86_64-linux/ghc-9.4.7/hamacs-0.0.1") . unGhcDbPath) =<< packageDatabase
    --   HI.unsafeRunInterpreterWithArgs
    --     [ "-no-user-package-db" -- , "-v"
    --     , "-package-env", "-"
    --     , "-package-db", "/home/dan/pro/haskell/my/hamacs/dist-newstyle/packagedb/ghc-9.12.2"
    --     , "-package", "base"
    --     , "-package", "hamacs"
    --     , "-package", "exceptions"
    --     , "-package", "filepath"
    --     , "-package", "cases"
    --     , "-package", "containers"
    --     , "-package", "mtl"
    --     , "-package", "hint"
    --     , "-package", "relude"
    --     , "-package", "stm"
    --     , "-package", "text"
    --     , "-package", "unliftio"
    --     , "-package", "unliftio-core"
    --     ] (
    --     let codeAsStr = toString hsCode in do
    --       afh <- accessFormHint <$> lift getPState
    --       putStrLn $ "Inside inter Eval [" <> codeAsStr <> "] accessFormHint = " <> toString afh
    --       HI.set [ HI.languageExtensions HI.:= [HI.NoImplicitPrelude] ]
    --       HI.loadModules [  "MyModule" ]
    --       HI.setImports [ "Emacs.Type", "MyModule", "Relude" ]
    --       -- () <- HI.runStmt codeAsStr -- (HI.as :: EmacsM ())
    --       -- r :: () <- HI.interpret codeAsStr (HI.as :: ())
    --       r :: EmacsM () <- HI.unsafeInterpret codeAsStr "EmacsM ()"
    --       putStrLn  "Eval Finished "
    --       lift r
    --     ) >>=
    --    \case
    --      Left e -> do
    --        putStrLn $ "Hint failed " <> show e
    --        pure 111
    --      Right () -> do
    --        putStrLn $ "Hint succes "
    --        pure 222
    -- ping :: TQueue HintReq -> EmacsM Int
    -- ping q = do
    --   message "Before putting Ping to Queue"
    --   atomically $ writeTQueue q PingHint
    --   pure 111
