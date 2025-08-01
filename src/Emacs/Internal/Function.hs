{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Emacs.Internal.Function where

import Data.Text qualified as T
import Data.Text.Foreign qualified as TF
import Emacs.Internal.Check
    ( checkExitStatus,
      _non_local_exit_throw,
      _non_local_exit_signal,
      nonLocalExitCheck,
      nonLocalExitSignal )
import Emacs.Internal.TypeOf ( isTypeOf )
import Emacs.Internal.Intern ( intern )
import Emacs.Internal.String ( mkString )

import Emacs.Internal.Nil ( mkNil, isNotNil )
import Emacs.Internal.Funcall ( funcall )
import Emacs.Prelude
import Emacs.Type

import Emacs.Type.ToEmacsValueInstances ()
import Emacs.Type.FromEmacsValueInstances ()
import Foreign.C.String ( CString )
import Foreign.C.Types ( CPtrdiff(..) )
import Foreign.Marshal.Array ( peekArray )
import Foreign.StablePtr ( StablePtr )
import GHC.Ptr ( FunPtr, nullPtr )
import Unsafe.Coerce ( unsafeCoerce )

-- TODO: arity と doc は Arity と Doc 型にするべきかな。
foreign import ccall _make_function
  :: EmacsEnv
  -> CPtrdiff
  -> CPtrdiff
  -> FunPtr EFunctionStub
  -> CString
  -> StablePtr a
  -> IO EmacsValue

-- TODO: ??? これ StablePtr の効果も兼ねている？
foreign import ccall "wrapper" wrapEFunctionStub
  :: EFunctionStub
  -> IO (FunPtr EFunctionStub)

stableNullPtr :: StablePtr Void
stableNullPtr = unsafeCoerce nullPtr

mkFunction :: (MonadIO m, HasEmacsCtx m) =>
  ([EmacsValue] -> NativeEmacsM EmacsValue) -> Int -> Int -> Text -> m EmacsValue
mkFunction f minArity' maxArity' doc' = do
  let minArity = fromIntegral minArity' :: CPtrdiff
      maxArity = fromIntegral maxArity' :: CPtrdiff
  stubp <- liftIO (wrapEFunctionStub stub)
  env <- getEmacsCtx
  checkExitStatus $ liftIO (TF.withCString doc' $ \emDoc ->
    _make_function env minArity maxArity stubp emDoc stableNullPtr)
  where
    stub :: EFunctionStub
    stub env nargs args _pstatep = do
      errorHandle env $ do
        es <- fmap EmacsValue <$> peekArray (fromIntegral nargs) args
        runNativeEmacsM env (f es)

errorHandle :: EmacsEnv -> IO EmacsValue -> IO EmacsValue
errorHandle env action =
  action `catch` emacsExceptionHandler
         `catch` haskellExceptionHandler
  where
    haskellExceptionHandler :: SomeException -> IO EmacsValue
    haskellExceptionHandler e =
      runNativeEmacsM env $ do
        funcallExit <- nonLocalExitCheck
        nil <- mkNil
        when (funcallExit == EmacsFuncallExitReturn) $ do
          mes <- mkString (toText $ displayException e)
          arg <- mkList [mes]
          sym <- intern "haskell-error"
          nonLocalExitSignal sym arg
        return nil

    emacsExceptionHandler :: EmacsException -> IO EmacsValue
    emacsExceptionHandler e@(EmacsException funcallExit a0 a1) = do
      let setter = case funcallExit of
                     EmacsFuncallExitSignal -> _non_local_exit_signal
                     EmacsFuncallExitThrow -> _non_local_exit_throw
                     _ -> error $ "Enexpected " <> show e
      setter env a0 a1
      return a0

mkFunctionFromCallable :: MonadEmacs m => NativeCallable f => f -> m EmacsValue
mkFunctionFromCallable f = do
  let a = arity f
  mkFunction func a a ""
  where
    func :: [EmacsValue] -> NativeEmacsM EmacsValue
    func es = do
      res <- natCall f es
      case res of
        Right ev -> return ev
        Left e -> fail $ "mkFunctionFromCallable failed for f with " <> show (length es) <> " args: " <> show e


instance {-# OVERLAPPING #-} ToEmacsValue a => CallableArity a where
    arity _ = 0

instance {-# OVERLAPPING #-} ToEmacsValue a => NativeCallable a where
    natCall a [] = Right <$> toEv a
    natCall _ _  = pure $ Left "Too many arguments"

instance {-# OVERLAPPING #-} ToEmacsValue a => CallableArity (IO a) where
    arity _ = 0


instance {-# OVERLAPPING #-} ToEmacsValue a => NativeCallable (IO a) where
    natCall a [] = do
      v <- liftIO a
      Right <$> toEv v
    natCall _ _  = pure $ Left "Too many arguments"

instance {-# OVERLAPPING #-} ToEmacsValue a => CallableArity (NativeEmacsM a) where
  arity _ = 0

instance {-# OVERLAPPING #-} ToEmacsValue a => NativeCallable (NativeEmacsM a) where
  natCall am [] = do
    a <- am
    Right <$> toEv a
  natCall _ _  = pure $ Left "Too many arguments"

instance {-# OVERLAPPING #-} (FromEmacsValue a, CallableArity b) => CallableArity (a -> b) where
  arity f = arity (f (error "NativeCallable (a -> b)")) + 1

instance {-# OVERLAPPING #-} (FromEmacsValue a, NativeCallable b) => NativeCallable (a -> b) where
  natCall f (e:es) = do
    av <- fromEv e
    natCall (f av) es
  natCall _ [] = pure $ Left "Too less arguments"

instance (FromEmacsValue a, NativeCallable b) => ToEmacsValue (a -> b) where
  toEv = (asEmacsValue<$>) . toEmacsFunction

instance ToEmacsFunction EmacsFunction where
  toEmacsFunction = pure

instance (FromEmacsValue a, NativeCallable b) => ToEmacsFunction (a -> b) where
  toEmacsFunction f = EmacsFunction <$> mkFunctionFromCallable f

funcall0 :: MonadEmacs m => Text -> m EmacsValue
funcall0 fname =
  join $ funcall <$> intern fname
                 <*> pure []

funcall1
  :: (MonadEmacs m, ToEmacsValue a)
  => Text
  -> a
  -> m EmacsValue
funcall1 fname ev0 =
  join $ funcall <$> intern fname
                 <*> sequence [toEv ev0]

funcall2
  :: (MonadEmacs m, ToEmacsValue a, ToEmacsValue b)
  => Text
  -> a
  -> b
  -> m EmacsValue
funcall2 fname ev0 ev1 =
  join $ funcall <$> intern fname
                 <*> sequence [toEv ev0, toEv ev1]

funcall3
  :: (MonadEmacs m, ToEmacsValue a, ToEmacsValue b, ToEmacsValue c)
  => Text
  -> a
  -> b
  -> c
  -> m EmacsValue
funcall3 fname ev0 ev1 ev2 =
  join $ funcall <$> intern fname
                 <*> sequence [toEv ev0, toEv ev1, toEv ev2]

mkCons
  :: (MonadEmacs m, ToEmacsValue a, ToEmacsValue b)
  => a
  -> b
  -> m EmacsCons
mkCons a b =
  EmacsCons <$> funcall2 "cons" a b

car :: MonadEmacs m => EmacsValue -> m EmacsValue
car = funcall1 "car"

cdr :: MonadEmacs m => EmacsValue -> m EmacsValue
cdr = funcall1 "cdr"

extractList :: MonadEmacs m => EmacsValue -> m [EmacsValue]
extractList ev = do
  isTypeOf ECons ev >>= \case
    False ->
      isTypeOf ENil ev >>= \case
        False ->
          throwIO . AssertionFailed . T.unpack =<< fromEv =<< funcall2 "format" ("Expected a list but got: %s" :: Text) ev
        True -> pure []
    True -> go ev
  where
    go l = do
      nonNil <- isNotNil l
      if nonNil
      then
        (:) <$> (car l) <*> (go =<< cdr l)
      else
        pure []

instance FromEmacsValue a => FromEmacsValue [a] where
  fromEv = mapM fromEv <=< extractList

mkList :: MonadEmacs m => [EmacsValue] -> m EmacsValue
mkList evs = do
  listQ <- intern "list"
  funcall listQ evs

instance ToEmacsValue h => ToEmacsValue [h] where
  toEv = (asEmacsValue<$>) . toEmacsList
instance ToEmacsList EmacsList where
  toEmacsList = pure
instance ToEmacsValue x => ToEmacsList [x] where
  toEmacsList xs = EmacsList <$> (mkList =<< mapM toEv xs)

instance MonadEmacs NativeEmacsM where
  callOverEmacs (EmacsSymbol s) actions = do
    pas <- mkFunctionFromCallable actions
    putStrLn $ "zzzzzzzzzzz " <> show pas
    fromEv =<< funcall1 "eval" (s : [pas])
