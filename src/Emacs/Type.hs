{-# LANGUAGE OverloadedStrings #-}
module Emacs.Type where

import Prelude(Show(..))
import Relude hiding (show)
import GHC.Ptr ( Ptr )
import Foreign.C.Types ( CInt, CPtrdiff )
import Foreign.StablePtr ( StablePtr )
import Foreign.Storable ( Storable )
import Data.Data ( Data(dataTypeOf), dataTypeConstrs, fromConstr )

-- data PState = PState
--   { symbolMap :: IORef (Map Text GlobalEmacsValue)
--   , accessFormHint :: Text
--   }

type Ctx = EmacsEnv

-- data Ctx = Ctx
--   { pstateStablePtr :: StablePtr PState
--   , pstate :: PState
--   , emacsEnv :: EmacsEnv
--   -- , rtPtr :: Ptr ()
--   }

class HasEmacsCtx m where
  getEmacsCtx :: m Ctx

returnHello :: Text
returnHello = "Hello"

type EmacsM =
  ReaderT Ctx IO

instance HasEmacsCtx EmacsM where
  getEmacsCtx = ask

-- getPState :: (MonadIO m, HasEmacsCtx m) => m PState
-- getPState = pstate <$> getEmacsCtx

data EmacsType = ESymbol
               | EInteger
               | EFunction
               | EString
               | ECons
               | ENil
  deriving (Show, Eq, Data)


emacsTypeSymbolName :: EmacsType -> Text
emacsTypeSymbolName ESymbol   = "symbol"
emacsTypeSymbolName EInteger  = "integer"
emacsTypeSymbolName EFunction = "function"
emacsTypeSymbolName EString   = "string"
emacsTypeSymbolName ECons     = "cons"
emacsTypeSymbolName ENil      = "nil" -- lie

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

-- AsEmacsValue
class    AsEmacsValue s             where asEmacsValue :: s -> EmacsValue
instance AsEmacsValue EmacsSymbol   where asEmacsValue (EmacsSymbol ev) = ev
instance AsEmacsValue EmacsKeyword  where asEmacsValue (EmacsKeyword ev) = ev
instance AsEmacsValue EmacsCons     where asEmacsValue (EmacsCons ev) = ev
instance AsEmacsValue EmacsList     where asEmacsValue (EmacsList ev) = ev
instance AsEmacsValue EmacsFunction where asEmacsValue (EmacsFunction ev) = ev

class Callable a where
    call :: a -> [EmacsValue] -> EmacsM (Either Text EmacsValue)
    arity :: a -> Int

class ToEmacsValue h where
  toEv :: (MonadIO m, HasEmacsCtx m) => h -> m EmacsValue

class FromEmacsValue h where
  fromEv :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> m h

class ToEmacsValue s => ToEmacsSymbol s where
  toEmacsSymbol :: (MonadIO m, HasEmacsCtx m) => s -> m EmacsSymbol

class ToEmacsValue s => ToEmacsCons s where
  toEmacsCons :: (MonadIO m, HasEmacsCtx m) => s -> m EmacsCons

class ToEmacsValue s => ToEmacsKeyword s where
  toEmacsKeyword :: (MonadIO m, HasEmacsCtx m) => s -> m EmacsKeyword

class ToEmacsValue s => ToEmacsList s where
  toEmacsList :: (MonadIO m, HasEmacsCtx m) => s -> m EmacsList

class (Callable s,ToEmacsValue s) => ToEmacsFunction s where
  toEmacsFunction :: (MonadIO m, HasEmacsCtx m) => s -> m EmacsFunction

-- Logging here is not a good idea. When passing high order function,
-- which could be invoked manytimes, its get quite slow.
runEmacsM :: MonadIO m => Ctx -> EmacsM a -> m a
runEmacsM ctx action =
  liftIO $ runReaderT action ctx
