{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Emacs.Hint.Start where


import Distribution.Types.PackageName ( unPackageName )
import Emacs.Type
import Emacs.Package.Cabal
import Emacs.Internal ()
import Emacs.Prelude
import Emacs.Text
import Language.Haskell.Interpreter (ModuleName, MonadInterpreter, ModuleElem (Fun), Id)
import Language.Haskell.Interpreter qualified as HI
import Language.Haskell.Interpreter.Unsafe qualified as HI
import Text.Pretty.Simple ( pPrintForceColor )
import UnliftIO.Exception ( catchAny )
import UnliftIO.STM ( TQueue )

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


runHintOn :: [Text] -> CabalFilePath -> TQueue HintReq -> NativeEmacsM ()
runHintOn customHintArgs cabalFile q = catchAny go oops
  where
    oops (SomeException se) = do
      putStrLn $ "\nLoading of " <> show cabalFile <> "failed due:\n"
      pPrintForceColor se
      -- writeTQueue failure to unlock main thread

    hintArgTweaks =
      case customHintArgs of
        [] ->
          [ "-no-user-package-db"
          , "-package-env", "-"
          , "-package-db", "/home/dan/pro/haskell/my/hamacs/dist-newstyle/packagedb/ghc-9.12.2"
          ]
        o -> fmap toString o
    hintImports = [ "UnliftIO.STM", "Emacs.Type", "Emacs.Core", "Emacs.Hint.Type", "Emacs.Hint", "Relude" ]
    go :: NativeEmacsM ()
    go =
      mkHamacsPackage <$> parseCabalFile cabalFile >>= \case
        Left e -> fail $ toString e
        Right hp -> do
          emcEnv <- ask
          let packName = toText $ unPackageName hp.packageName
          putStrLn $ "HINT ARGS " <> show (hamacsPackageToHintArgs cabalFile hp) <> "\n" <>  show (modulesExportedToHint hp)
          HI.unsafeRunInterpreterWithArgs
            (hintArgTweaks <> hamacsPackageToHintArgs cabalFile hp)
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
                  let cmd :: Text = "defunHint \"" <> packName <> "-" <>
                        toText (trimX '(' ')' fnName) <> "\" " <> toText fqFnName
                  r2 :: EmacsM () <- HI.unsafeInterpret (toString cmd)  "EmacsM ()"
                  liftIO (runReaderT r2 $ Ctx emcEnv q)

              r :: HintQueueRunner <- HI.unsafeInterpret "runHintQueue" "HintQueueRunner"
              liftIO (runReaderT r q)
            ) >>=
           \case
             Left e -> do
               putLText "Hint failed: "
               pPrintForceColor e
             Right ev -> do
               putStrLn $ show cabalFile <> " is exited " <> show ev
