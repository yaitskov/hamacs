{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Emacs.Hint.Type where


import Emacs.Core
import Emacs.Prelude
import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import UnliftIO.Concurrent (ThreadId)
import UnliftIO.STM (TQueue)

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
