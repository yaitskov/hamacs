{-# LANGUAGE OverloadedStrings #-}
module Emacs.Type
  ( module Emacs.Type
  , module X
  ) where


import Emacs.Type.Annotations as X
import Emacs.Prelude hiding (show)
import Foreign.C.Types ( CPtrdiff, CInt )
import Foreign.Ptr ( Ptr )
import Foreign.StablePtr ( StablePtr )
import UnliftIO.STM (TQueue)
import Foreign.Storable ( Storable )
import Prelude (Show(..))


data HintReq
  = PutMVarOnReady (MVar ())
  | CallFun
      ([EmacsValue] -> EmacsM EmacsValue)
      [EmacsValue]
      (MVar (Either SomeException EmacsValue))
      EmacsEnv
  | ReThrow SomeException
  | Return EmacsValue


type HintQueueRunner = ReaderT (TQueue HintReq) IO EmacsValue

data Ctx
  = Ctx
  { emEnv :: EmacsEnv
  , hintQueue :: TQueue HintReq
  } deriving (Generic)

class HasEmacsCtx m where
  getEmacsCtx :: m EmacsEnv

type EmacsM = ReaderT Ctx IO

instance HasEmacsCtx EmacsM where
  getEmacsCtx = asks emEnv

data EmacsType = ESymbol
               | EInteger
               | EFunction
               | EString
               | ECons
               | ENil
  deriving (Show, Eq, Data)

emacsTypeSymbolName :: EmacsType -> Text
emacsTypeSymbolName =
  \case
    ESymbol   -> "symbol"
    EInteger  -> "integer"
    EFunction -> "function"
    EString   -> "string"
    ECons     -> "cons"
    ENil      -> "nil"

emacsTypes :: [EmacsType]
emacsTypes = fromConstr <$> dataTypeConstrs (dataTypeOf ESymbol)

type EmacsModule = Ptr () -> IO CInt

newtype EmacsEnv   = EmacsEnv (Ptr ())
  deriving (Storable, Show)

newtype EmacsValue = EmacsValue (Ptr ())
  deriving (Storable, Show)

newtype GlobalEmacsValue = GlobalEmacsValue (Ptr ())
  deriving (Storable)

castGlobalToEmacsValue :: GlobalEmacsValue -> EmacsValue
castGlobalToEmacsValue (GlobalEmacsValue p) =
  EmacsValue p

newtype EmacsSymbol   = EmacsSymbol   { unEmacsSymbol :: EmacsValue }
newtype EmacsKeyword  = EmacsKeyword  EmacsValue
newtype EmacsCons     = EmacsCons     EmacsValue
newtype EmacsFunction = EmacsFunction EmacsValue
newtype EmacsList     = EmacsList     EmacsValue

newtype Symbol = Symbol Text
newtype Keyword = Keyword Text
data Cons = Cons EmacsValue EmacsValue

data EmacsFuncallExit
  = EmacsFuncallExitReturn
  | EmacsFuncallExitSignal
  | EmacsFuncallExitThrow
  deriving (Show,Eq,Enum)

data EmacsException
  = EmacsException EmacsFuncallExit EmacsValue EmacsValue

instance Show EmacsException where
  show (EmacsException funcallExit _ _) =
    "EmacsException(" <> show funcallExit <> ")"

instance Exception EmacsException

type EFunctionStub
  = EmacsEnv
  -> CPtrdiff
  -> Ptr (Ptr ())
  -> StablePtr Void
  -> IO EmacsValue

data InteractiveForm = InteractiveNoArgs

newtype EmDoc = EmDoc Text
newtype Arity = Arity Int

class    AsEmacsValue s             where asEmacsValue :: s -> EmacsValue
instance AsEmacsValue EmacsSymbol   where asEmacsValue (EmacsSymbol ev) = ev
instance AsEmacsValue EmacsKeyword  where asEmacsValue (EmacsKeyword ev) = ev
instance AsEmacsValue EmacsCons     where asEmacsValue (EmacsCons ev) = ev
instance AsEmacsValue EmacsList     where asEmacsValue (EmacsList ev) = ev
instance AsEmacsValue EmacsFunction where asEmacsValue (EmacsFunction ev) = ev

type NativeEmacsM = ReaderT EmacsEnv IO

instance HasEmacsCtx NativeEmacsM where
  getEmacsCtx = ask

class (HasEmacsCtx m, MonadFail m, MonadUnliftIO m) => MonadEmacs m where
  callOverEmacs :: (FromEmacsValue a, ToEmacsValue a) => EmacsSymbol -> m a -> m a

class CallableArity a where
    arity :: a -> Int

class CallableArity a => NativeCallable a where
    natCall :: a -> [EmacsValue] -> NativeEmacsM (Either Text EmacsValue)

class ToEmacsValue h where
  toEv :: MonadEmacs m => h -> m EmacsValue

class FromEmacsValue h where
  fromEv :: MonadEmacs m => EmacsValue -> m h

class ToEmacsValue s => ToEmacsSymbol s where
  toEmacsSymbol :: MonadEmacs m => s -> m EmacsSymbol

class ToEmacsValue s => ToEmacsCons s where
  toEmacsCons :: MonadEmacs m => s -> m EmacsCons

class ToEmacsValue s => ToEmacsKeyword s where
  toEmacsKeyword :: MonadEmacs m => s -> m EmacsKeyword

class ToEmacsValue s => ToEmacsList s where
  toEmacsList :: MonadEmacs m => s -> m EmacsList

class (NativeCallable s,ToEmacsValue s) => ToEmacsFunction s where
  toEmacsFunction :: MonadEmacs m => s -> m EmacsFunction

-- Logging here is not a good idea. When passing high order function,
-- which could be invoked manytimes, its get quite slow.
runNativeEmacsM :: MonadIO m => EmacsEnv -> NativeEmacsM a -> m a
runNativeEmacsM ctx action =
  liftIO $ runReaderT action ctx
