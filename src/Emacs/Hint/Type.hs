{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
module Emacs.Hint.Type where

import Data.FileEmbed
import Emacs.Core
import Emacs.Prelude
import Foreign.C.Types ( CPtrdiff )
import Foreign.Ptr ( Ptr )
import Foreign.StablePtr ( StablePtr )
import UnliftIO.Concurrent (ThreadId)
import UnliftIO.STM (TQueue)

genNixGhcLibDirFileContent :: ByteString
genNixGhcLibDirFileContent = $(embedFile "src/gen-nix-ghc-libdir.nix")

type HintFunctionStub
  = EmacsEnv
  -> CPtrdiff
  -> Ptr (Ptr ())
  -> StablePtr ()
  -> IO EmacsValue


data Hint
  = Hint
    { hintQueue :: TQueue HintReq
    , hintThreadId :: ThreadId
    }

newtype GhcDbPath = GhcDbPath { unGhcDbPath :: FilePath }  deriving (Show, Eq)

data EmacsFunAnnotations
  = EmacsFunAnnotations
    { isInteractive :: Maybe Interactive
    , emDocString :: EmDocString
    } deriving (Show, Eq, Generic)

data DefunHintCtx = DefunHintCtx Ctx EmacsFunAnnotations deriving (Generic)
