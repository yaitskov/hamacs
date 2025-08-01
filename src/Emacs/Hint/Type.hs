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

type EmacsHintM = EmacsM

-- data EmacsHintConf
--   = EmacsHintConf
--   { packageName :: Text
--   , packageInQueue :: TQueue HintReq
--   , emacsCtxM :: Ctx
--   }

-- newtype EmacsHintM a = EmacsHintM (ReaderT EmacsHintConf IO a)
--   deriving newtype (Applicative, Functor, Monad, MonadIO, MonadUnliftIO)

-- instance MonadReader EmacsHintConf EmacsHintM where
--    ask = EmacsHintM ask
--    local f (EmacsHintM m) = EmacsHintM (local f m)

-- instance HasEmacsCtx EmacsHintM where
--   getEmacsCtx = EmacsHintM (asks (.emacsCtxM))

-- data HintQueueWorkerConf
--   = HintQueueWorkerConf
--   { packageName :: Text
--   , packageInQueue :: TQueue HintReq
--   }
