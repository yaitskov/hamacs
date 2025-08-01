{-# LANGUAGE OverloadedStrings #-}

module Emacs.Init where

import Data.Map.Strict qualified as M
-- import Data.Time.Clock.System
--     ( SystemTime(systemSeconds), getSystemTime )
import Data.Text qualified as T
import Emacs (defmodule, intern, defun )
import Emacs.Api.Native.Package ( loadPath, require )
-- import Emacs.Api.Native.String
-- import Emacs.Api.Native.Buffer
import Emacs.Hint.Start ( runHintOn )

import Emacs.Type
import Emacs.Internal ()
import Emacs.Package.Cabal ( CabalFilePath(..) )
import Emacs.Prelude
import Foreign.C.Types ( CInt(CInt) )
import System.FilePath ( (</>), (<.>) )
import System.IO.Unsafe ( unsafePerformIO )
import UnliftIO.Concurrent ( forkIO, modifyMVar_)
import UnliftIO.Directory ( doesFileExist )
-- import UnliftIO.Environment ( lookupEnv )
import UnliftIO.STM ( writeTQueue, atomically, newTQueueIO, TQueue )

foreign export ccall "emacs_module_init" emacsModuleInit :: EmacsModule

-- packageDatabase :: MonadIO m => m [GhcDbPath]
-- packageDatabase =
--   lookupEnv "NIX_GHC_LIBDIR"
--   <&> fmap (GhcDbPath . (</> "package.conf.d")) . maybeToList
--   >>= filterM (doesDirectoryExist . unGhcDbPath)

hamacsPackageQueues :: MVar (Map Text (TQueue HintReq))
hamacsPackageQueues = unsafePerformIO (newMVar mempty)

findHamacsPackageCabal :: MonadEmacs m => Text -> m CabalFilePath
findHamacsPackageCabal packNameTxt = do
  candidiates <- findMOf each (\p -> doesFileExist . toString $ p </> packName </> packName <.> "cabal") . fmap T.unpack =<< loadPath
  case candidiates of
    Nothing -> fail $ "Package " <> packName <> " is not found in load-path"
    Just x -> pure . CabalFilePath $ x </> packName </> packName <.> "cabal"
  where
    packName = T.unpack packNameTxt

emacsModuleInit :: EmacsModule
emacsModuleInit = defmodule "hamacs" $ do
  mapM_ (require <=< intern) ["subr-x", "package"]
  -- defun "hamacs-test-save-excursion" seTest

  defun "hamacs-load-package" loadHamacsPackage
  where
    -- seTest :: EmacsM ()
    -- seTest = do
    --   void $ message "Before Save"
    --   -- se <- intern "save-excursion"
    --   void $ saveExcursion (message "Inside Save")
    --   void $ message "After Save"

    -- pingPackage :: Text -> EmacsM ()
    -- pingPackage pkgName = do
    --   pm <- readMVar hamacsPackageQueues
    --   case M.lookup pkgName pm of
    --     Nothing -> throwIO . AssertionFailed $ "Hamacs package [" <> show pkgName <> "] is not found"
    --     Just pkgQueue -> do
    --       em <- newEmptyMVar
    --       beforeCall <- liftIO (systemSeconds <$> getSystemTime)
    --       atomically . writeTQueue pkgQueue $ SyncPing pkgName em
    --       takeMVar em
    --       afterRespond <- liftIO (systemSeconds <$> getSystemTime)
    --       putTextLn $ "Duration: " <> show (afterRespond - beforeCall) <> " seconds"

    loadHamacsPackage :: Text -> NativeEmacsM Text
    loadHamacsPackage hmPgkName = do
      cabalFile <- findHamacsPackageCabal hmPgkName
      pkgQueue <- newTQueueIO
      responseMvar <- newEmptyMVar
      atomically . writeTQueue pkgQueue $ PutMVarOnReady responseMvar
      tid <- forkIO $ runHintOn cabalFile pkgQueue

      putStr $ "Pending Hamacs package " <> show hmPgkName <> " from tread " <> show tid <> " while loading is complete..."
      takeMVar responseMvar
      putTextLn $ "  Loading of " <> hmPgkName <> " is complete"

      modifyMVar_ hamacsPackageQueues (pure . M.insert hmPgkName pkgQueue)
      pure $ "package [" <> hmPgkName <> "] is ready"
