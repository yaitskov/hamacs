{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Emacs.Hint.Start where

import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.Text.IO qualified as T
import Data.Version ( showVersion )
import Distribution.Types.PackageName ( unPackageName )
import Emacs.Package.Cabal
    ( CabalFilePath (..),
      HamacsPackage(packageName),
      mkHamacsPackage,
      parseCabalFile,
      hamacsPackageToHintArgs,
      modulesExportedToHint )
import Emacs.Internal ()
import Emacs.Hint.Type
import Emacs.Prelude
import Emacs.Text ( trimX )
import Emacs.Type ( HintReq, NativeEmacsM, Ctx(Ctx), HintQueueRunner )
import Language.Haskell.Interpreter (ModuleName, MonadInterpreter, ModuleElem (Fun), Id)
import Language.Haskell.Interpreter qualified as HI
import Language.Haskell.Interpreter.Unsafe qualified as HI
import System.FilePath ( (</>), dropFileName )
import System.Info ( fullCompilerVersion )
import System.Process (callProcess)
import Text.Pretty.Simple ( pPrintForceColor )
import UnliftIO.Exception ( catchAny )
import UnliftIO.STM ( TQueue )
import UnliftIO.IO ( withBinaryFile )
import UnliftIO.Temporary ( withSystemTempDirectory )
import UnliftIO.Directory ( doesFileExist )

modElemToFun :: ModuleElem -> Maybe Id
modElemToFun = \case Fun x -> Just x ; _ -> Nothing

modFunctions :: MonadInterpreter m => ModuleName -> m [Id]
modFunctions mm = mapMaybe modElemToFun <$> HI.getModuleExports mm

funInfo :: MonadInterpreter m => Id -> m EmacsFunAnnotations
funInfo fName = do
  EmacsFunAnnotations
    <$> (listToMaybe <$> HI.getValAnnotations undef fName)
    <*> (mconcat <$> HI.getValAnnotations undef fName)
  where
    undef :: forall a . a
    undef = error $ "fail to gen annotations for " <> toText fName

isEmacsCompatibleFunction :: MonadInterpreter m => Id -> m Bool
isEmacsCompatibleFunction fName =
  HI.typeChecksWithDetails ("mkHintFunctionFromCallable mempty " <> fName) >>= \case
    Left es -> do
      putStrLn $ "Skip " <> fName <> " due: " <> show es
      pure False
    Right ok -> do
      putStrLn $ "Pass " <> fName <> " as " <> ok
      pure True


shortGhcVersion :: String
shortGhcVersion = "ghc" <> filter isDigit (showVersion fullCompilerVersion)

mkNixGhcLibDir :: CabalFilePath -> NativeEmacsM Text
mkNixGhcLibDir (CabalFilePath cfp) =
  withSystemTempDirectory "hamacs-nix" $ \hnd -> liftIO do
    let nixGhcLibDir = hnd </> "NIXGHCLIBDIR"
        libNixFile = dropFileName cfp </> "nix-ghc-libdir-setup.nix"
    nixShellArgs <- doesFileExist libNixFile >>= \case
      True ->
        pure [ "--argstr"
             , "nix-ghc-libdir-out-file"
             , nixGhcLibDir
             , "--argstr"
             , "ghc"
             , shortGhcVersion
             , "--run"
             , "gen_nix_ghc_libdir"
             , "--extra-experimental-features"
             , "nix-command"
             , "--extra-experimental-features"
             , "pipe-operators"
             , libNixFile
             ]
      False -> do
        let nixGenFile = hnd </> "gen-nix-ghc-libdir.nix"
        withBinaryFile nixGenFile WriteMode $ \h -> BS.hPut h genNixGhcLibDirFileContent
        pure [ "--argstr"
             , "nix-ghc-libdir-out-file"
             , nixGhcLibDir
             , "--argstr"
             , "ghc"
             , shortGhcVersion
             , "--argstr"
             , "cabal-project-name"
             , cfp
             , "--run"
             , "gen_nix_ghc_libdir"
             , "--verbose"
             , "--extra-experimental-features"
             , "nix-command"
             , "--extra-experimental-features"
             , "pipe-operators"
             , nixGenFile
             ]

    putStrLn $ "NIX gen SHELL  args " <> show nixShellArgs
    callProcess "nix-shell" nixShellArgs
    withBinaryFile nixGhcLibDir ReadMode $ \h -> T.hGetContents h

runHintOn :: [Text] -> CabalFilePath -> TQueue HintReq -> NativeEmacsM ()
runHintOn customHintArgs cabalFile q = catchAny go oops
  where
    oops (SomeException se) = do
      putStrLn $ "\nLoading of " <> show cabalFile <> " failed due:\n"
      pPrintForceColor se

    mkHintArgs hp = do
      ha <- (<> hamacsPackageToHintArgs cabalFile hp) <$>
            if customHintArgs == []
            then do nixGhcLibDir <- mkNixGhcLibDir cabalFile
                    pure [ "-no-user-package-db"
                         , "-package-env", "-"
                         , "-package-db", toString nixGhcLibDir
                         ]
            else pure $ fmap toString customHintArgs
      putStrLn $ "HINT ARGS " <> show ha <> "\n" <> show (modulesExportedToHint hp)
      pure ha

    hintImports = [ "UnliftIO.STM", "Emacs.Type", "Emacs.Core", "Emacs.Hint.Type", "Emacs.Hint", "Relude" ]
    go :: NativeEmacsM ()
    go =
      mkHamacsPackage <$> parseCabalFile cabalFile >>= \case
        Left e -> fail $ toString e
        Right hp -> do
          emcEnv <- ask
          let packName = toText $ unPackageName hp.packageName
          hintArgs <- mkHintArgs hp
          HI.unsafeRunInterpreterWithArgs
            hintArgs
            (do
              HI.set [ HI.languageExtensions HI.:= [HI.NoImplicitPrelude] ]
              HI.loadModules $ modulesExportedToHint hp
              HI.setImports $ hintImports <> modulesExportedToHint hp

              putStrLn $ "before unsafeInterpret"

              forM_ (modulesExportedToHint hp) $ \exMod -> do
                fqFnNames <- fmap (\fname -> ( fname
                                             , case fname of
                                                 '(' : fnameTail -> '(' : exMod <> "." <> fnameTail
                                                 _               -> exMod <> "." <> fname
                                             )
                                  ) <$> modFunctions exMod
                -- printDoc $ hsep ["Functions of",  doc exportingModule, ":"
                --                   <> linebreak <> tab (vsep fnNames) <> linebreak
                --                 ]
                emacsCompatibleOnes <- filterM (isEmacsCompatibleFunction . snd) fqFnNames
                printDoc $ vsep ["Among them Emacs compatible:", tab (vsep emacsCompatibleOnes)] <> linebreak

                forM_ emacsCompatibleOnes $ \(fnName, fqFnName) -> do
                  finf <- funInfo fqFnName
                  let cmd :: Text = "defunHint \"" <> packName <> "-" <>
                        toText (trimX '(' ')' fnName) <> "\" " <> toText fqFnName
                  r2 :: ReaderT DefunHintCtx IO () <- HI.unsafeInterpret (toString cmd) "ReaderT DefunHintCtx IO ()"
                  liftIO (runReaderT r2 (DefunHintCtx (Ctx emcEnv q) finf))

              r :: HintQueueRunner <- HI.unsafeInterpret "runHintQueue" "HintQueueRunner"
              liftIO (runReaderT r q)
            ) >>=
           \case
             Left e -> do
               putLText "Hint failed: "
               pPrintForceColor e
             Right ev -> do
               putStrLn $ show cabalFile <> " is exited " <> show ev
