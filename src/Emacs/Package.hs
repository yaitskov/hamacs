{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Emacs.Package where

-- import UnliftIO.Concurrent
import Data.Aeson as A
import Emacs.Prelude
import Emacs.Validation
-- import Emacs.Type
-- import System.FilePath
-- import System.Directory

-- import Language.Haskell.Interpreter qualified as HI
-- import Language.Haskell.Interpreter.Unsafe qualified as HI

-- import UnliftIO.STM
-- import UnliftIO.Exception


newtype HaskellFunctionName = HaskellFunctionName Text
  deriving (Generic)
  deriving newtype (Show, Eq, Ord, IsString, NFData, FromJSON, ToJSON)

newtype CabalPackageName = CabalPackageName Text
  deriving (Generic)
  deriving newtype (Show, Eq, Ord, IsString, NFData, FromJSON, ToJSON)

newtype HaskellModuleName = HaskellModuleName Text
  deriving (Generic)
  deriving newtype (Show, Eq, Ord, IsString, NFData, FromJSON, ToJSON)

data PackageMetaFileG a
  = PackageMetaFile
  { version :: Columnar a (GreaterThan 0) Int
  , initFunction :: Columnar a IdPred HaskellFunctionName
  , cabalPackages :: Columnar a IdPred [CabalPackageName]
  , rootDirs :: Columnar a IdPred [FilePath]
  , ghcDbs :: Columnar a IdPred [FilePath]
  , userPackageDb :: Columnar a IdPred Bool
  , packageEnvs :: Columnar a IdPred [Text]
  , importModules :: Columnar a IdPred [HaskellModuleName]
  , languageExtensions :: Columnar a IdPred [Text]
  , loadModules :: Columnar a IdPred [HaskellModuleName]
  }

type PackageMetaFile = PackageMetaFileG Identity
type PackageMetaFileMaybe = PackageMetaFileG Maybe

deriving instance Generic PackageMetaFileMaybe
deriving instance Generic PackageMetaFile
deriving instance Show PackageMetaFile
deriving instance Eq PackageMetaFile
instance ToJSON PackageMetaFile where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON PackageMetaFile
deriving instance Show PackageMetaFileMaybe
deriving instance Eq PackageMetaFileMaybe
instance ToJSON PackageMetaFileMaybe where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON PackageMetaFileMaybe

instance Semigroup PackageMetaFileMaybe where
  a <> b =
    PackageMetaFile
    (a.version <|> b.version)
    (a.initFunction <|> b.initFunction)
    (a.cabalPackages <> b.cabalPackages)
    (a.rootDirs <|> b.rootDirs)
    (a.ghcDbs <|> b.ghcDbs)
    (a.userPackageDb <|> b.userPackageDb)
    (a.packageEnvs <|> b.packageEnvs)
    (a.importModules <|> b.importModules)
    (a.languageExtensions <|> b.languageExtensions)
    (a.loadModules <|> b.loadModules)


-- defaultPackageMetaFile :: MonadIO m => FilePath -> m PackageMetaFileMaybe
-- defaultPackageMetaFile packageDir = do
--   modules <- findModulesInDir packageDir
--   pure
--     PackageMetaFile
--     { version = Nothing
--     , initFunction = pure "initHamacsPackage"
--     , cabalPackages = pure [ "relude", "hamacs" ]
--     , rootDirs = pure [ packageDir ]
--     , ghcDbs = pure [ "/home/dan/pro/haskell/my/hamacs/dist-newstyle/packagedb/ghc-9.12.2" ]
--     , userPackageDb = pure False
--     , packageEnvs = pure [ "-" ]
--     , importModules = pure [ "Relude", "Emacs" ]
--     , languageExtensions = pure [ "OverloadedStrings" ]
--     , loadModules = pure modules
--     }

-- findModulesInDir :: MonadIO m => FilePath -> m [ HaskellModuleName ]
-- findModulesInDir dir = mapMaybe (go . splitExtension) <$> liftIO (listDirectory dir)
--   where
--     go (fName, ext)
--       | ext == ".hs" = pure . HaskellModuleName $ toText fName
--       | otherwise = Nothing

-- readHamacsPackageFromDir :: MonadIO m => FilePath -> m PackageMetaFile
-- readHamacsPackageFromDir dir = _


-- initHamacsPackage :: PackageMetaFile -> EmacsM ()
-- initHamacsPackage pmf = do
--   pkgEvenQueue <- newTQueueIO
--   forkIO (packageWorkerThread pmf pkgEvenQueue)


-- data Event

-- packageWorkerThread :: PackageMetaFile -> EmacsM (TQueue Event)
-- packageWorkerThread p = _
