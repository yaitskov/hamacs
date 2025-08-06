{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
module Emacs.Init where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Emacs (defmodule, intern, defun )
import Emacs.Api.Native.Package ( loadPath, require )
import Emacs.Hint.Start ( runHintOn )
import Emacs.Type
    ( EmacsModule, HintReq(PutMVarOnReady), MonadEmacs, NativeEmacsM )
import Emacs.Internal ()
import Emacs.Package.Cabal ( CabalFilePath(..) )
import Emacs.Prelude hiding ((<.>))
import Foreign.C.Types ( CInt(CInt) )
import System.FilePath ( (</>), (<.>) )
import System.IO.Unsafe ( unsafePerformIO )
import UnliftIO.Concurrent ( forkIO, modifyMVar_)
import UnliftIO.Directory ( doesFileExist, makeAbsolute )
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
    Just x -> CabalFilePath <$> makeAbsolute (x </> packName </> packName <.> "cabal")
  where
    packName = T.unpack packNameTxt

emacsModuleInit :: EmacsModule
emacsModuleInit = defmodule "hamacs" $ do
  mapM_ (require <=< intern) ["subr-x", "package"]
  defun "hamacs-load-package" loadHamacsPackage
    """hamacs-load-package - load Hamacs package and its dependencies.
       It bind function defined in exporting modules
       prefixed with the package name and dash.
    """

  defun "hamacs-load-package-with-hint-args" loadHamacsPackageWithHintArgs
    """hamacs-load-package-with-hint-args - is used only for testing.
       Dependency resolution and hint argument generaction (i.e package-db) is
       disabled for packages loaded by this function.
    """
  where
    loadHamacsPackage :: Text -> NativeEmacsM Text
    loadHamacsPackage = loadHamacsPackageWithHintArgs []

    loadHamacsPackageWithHintArgs :: [Text] -> Text -> NativeEmacsM Text
    loadHamacsPackageWithHintArgs customHintArgs hmPgkName = do
      cabalFile <- findHamacsPackageCabal hmPgkName
      pkgQueue <- newTQueueIO
      responseMvar <- newEmptyMVar
      atomically . writeTQueue pkgQueue $ PutMVarOnReady responseMvar
      tid <- forkIO $ runHintOn customHintArgs cabalFile pkgQueue

      putStr $ "Pending Hamacs package " <> show hmPgkName <> " from tread " <> show tid <> " while loading is complete..."
      takeMVar responseMvar
      putTextLn $ "  Loading of " <> hmPgkName <> " is complete"

      modifyMVar_ hamacsPackageQueues (pure . M.insert hmPgkName pkgQueue)
      pure $ "package [" <> hmPgkName <> "] is ready"
