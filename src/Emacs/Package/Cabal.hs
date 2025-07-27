{-# LANGUAGE OverloadedRecordDot #-}

-- | hamacs package is described by a regular cabal file.
-- Cabal packaging allows uploading Hamacs packages on Hackage
module Emacs.Package.Cabal where

import Distribution.PackageDescription.Configuration
    ( flattenPackageDescription )
import Distribution.ModuleName ( components, ModuleName )
import Distribution.Simple.PackageDescription as PD
    ( readGenericPackageDescription )
import Distribution.Types.BuildInfo
    ( BuildInfo(defaultExtensions, targetBuildDepends, hsSourceDirs,
                defaultLanguage) )
import Distribution.Types.PackageDescription
    ( PackageDescription(package, library) )
import Distribution.Types.PackageId ( PackageIdentifier(pkgName) )
import Distribution.Types.PackageName
    ( PackageName, unPackageName )
import Distribution.Types.Library
    ( Library(exposedModules, libBuildInfo) )
import Distribution.Types.Dependency ( depPkgName )
import Distribution.Utils.Path
import Distribution.Verbosity ( verbose )
import Language.Haskell.Extension (Language (..), Extension (..))
import System.FilePath (dropFileName)
import Emacs.Prelude

data HamacsPackage
  = HamacsPackage
  { dependencies :: [ PackageName ]
  , packageName :: PackageName
  , sourceDirs :: [ SymbolicPath Pkg ('Dir Source) ]
  , language :: Language
  , extensions :: [Extension]
  , exposedModules :: [ ModuleName ]
  } deriving (Show, Eq)

newtype CabalFilePath = CabalFilePath FilePath deriving newtype (Show, Eq, Semigroup, Monoid)

parseCabalFile :: MonadIO m => CabalFilePath -> m PackageDescription
parseCabalFile (CabalFilePath cabPath) =
  flattenPackageDescription <$> liftIO (PD.readGenericPackageDescription verbose Nothing (makeSymbolicPath cabPath))


mkHamacsPackage :: PackageDescription -> Either Text HamacsPackage
mkHamacsPackage pd = do
  case pd.library of
    Nothing -> fail "No default library"
    Just dl ->
      pure $ HamacsPackage
      { dependencies = depPkgName <$> dl.libBuildInfo.targetBuildDepends
      , packageName = pd.package.pkgName
      , sourceDirs = dl.libBuildInfo.hsSourceDirs
      , language = fromMaybe GHC2024 dl.libBuildInfo.defaultLanguage
      , extensions = dl.libBuildInfo.defaultExtensions
      , exposedModules = dl.exposedModules
      }

class HasHintArgs a where
  getHintArgs :: a -> [String]

instance HasHintArgs PackageName where
  getHintArgs pn = ["-package", unPackageName pn]

instance HasHintArgs Language where
  getHintArgs l = ["-X" <> show l]

instance HasHintArgs Extension where
  getHintArgs e = ["-X" <> showE e]
    where
      showE = \case
        EnableExtension ee -> show ee
        DisableExtension de -> "No" <> show de
        UnknownExtension ue -> ue

hamacsPackageToHintArgs :: CabalFilePath -> HamacsPackage -> [String]
hamacsPackageToHintArgs (CabalFilePath cabPath) hp =
  concatMap getHintArgs hp.dependencies <>
  getHintArgs hp.language <>
  concatMap getHintArgs hp.extensions <>
  fmap ((("-i" <> dropFileName cabPath) </>) . getSymbolicPath) hp.sourceDirs

modulesExportedToHint :: HamacsPackage -> [String]
modulesExportedToHint hp = intercalate "." . components <$> hp.exposedModules
