module Emacs.Hint where


import Emacs.Prelude
import Emacs.Type
import UnliftIO.Concurrent ( ThreadId )
import UnliftIO.STM ( TQueue, readTQueue, atomically)

data HintReq
  = EvalHsCode Text
  | PingHint
  | PutMVarOnReady (MVar ())
  | KillHint deriving ( Eq)

data Hint
  = Hint
    { hintQueue :: TQueue HintReq
    , hintThreadId :: ThreadId
    }

newtype GhcDbPath = GhcDbPath { unGhcDbPath :: FilePath }  deriving (Show, Eq)


runHintQueue :: StateT (TQueue HintReq) EmacsM ()
runHintQueue = do
  q <- get
  atomically (readTQueue q) >>= \case
    PutMVarOnReady m -> do
      putStrLn "Before put () to hint MVAR"
      putMVar m ()
      runHintQueue
    PingHint -> putStrLn "Hint Worker is still alive" >> runHintQueue
    KillHint -> putStrLn "Dead fish"
    -- afh <- accessFormHint <$> lift getPState
    -- putStrLn $ "Inside inter Eval [" <> codeAsStr <> "] accessFormHint = " <> toString afh

    EvalHsCode code ->
      let codeAsStr = toString code in do
        putStrLn $ "Eval inside Hint [" <> codeAsStr <> "]"
        -- r :: EmacsM () <- HI.unsafeInterpret codeAsStr "EmacsM ()"
        -- lift r
        -- runHintQueue
