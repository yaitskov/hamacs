{-# LANGUAGE ForeignFunctionInterface, UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Emacs.Core (
    module Emacs.Internal,
    defmodule,
    mkCons,
    ToEmacsValue(..),
    ToEmacsSymbol(..),
    ToEmacsFunction(..),
    funcall1, funcall2, funcall3,
    mkFunctionFromCallable,
    Callable(..),
    --
    car,
    cdr,
    --
    evalString,
    provide,
    message,
    print,
    ) where

import Prelude()

import Relude  hiding (print)

import Emacs.Type
import Emacs.Internal


defmodule :: Text -> EmacsM a -> EmacsModule
defmodule name mod' ert = do
  env <- getEmacsEnvFromRT ert
  _ <- errorHandle env $ do
    ctx <- initCtx env
    runEmacsM ctx $ mod' >> funcall1 "provide" (Symbol name)
  return 0

class ToEmacsValue h where
  toEv :: (MonadIO m, HasEmacsCtx m) => h -> m EmacsValue

instance ToEmacsValue EmacsValue where
  toEv = pure

-- Integer
instance ToEmacsValue Int where
  toEv = mkInteger

-- String
instance ToEmacsValue Text where
  toEv = mkString

-- Symbol
instance ToEmacsValue EmacsSymbol where
  toEv = pure . asEmacsValue
instance ToEmacsValue Symbol where
  toEv = (asEmacsValue<$>) . toEmacsSymbol

-- Kwyword
instance ToEmacsValue EmacsKeyword where
  toEv = pure . asEmacsValue
instance ToEmacsValue Keyword where
  toEv = (asEmacsValue<$>) . toEmacsKeyword

-- Bool
instance ToEmacsValue Bool where
  toEv True  = mkT
  toEv False = mkNil

-- Nil
instance ToEmacsValue () where
  toEv _ = mkNil

-- List
instance ToEmacsValue EmacsList where
  toEv = pure . asEmacsValue
instance ToEmacsValue h => ToEmacsValue [h] where
  toEv = (asEmacsValue<$>) . toEmacsList

-- Cons
instance ToEmacsValue EmacsCons where
  toEv = pure . asEmacsValue
instance (ToEmacsValue a, ToEmacsValue b) => ToEmacsValue (a, b) where
  toEv = (asEmacsValue<$>) . toEmacsCons

-- Function
-- Can only handle function with no arguments.
-- Use mkFunctionFromCallable for no args.
instance ToEmacsValue EmacsFunction where
  toEv = pure . asEmacsValue
instance (FromEmacsValue a, Callable b) => ToEmacsValue (a -> b) where
  toEv = (asEmacsValue<$>) . toEmacsFunction

-- AsEmacsValue
class    AsEmacsValue s             where asEmacsValue :: s -> EmacsValue
instance AsEmacsValue EmacsSymbol   where asEmacsValue (EmacsSymbol ev) = ev
instance AsEmacsValue EmacsKeyword  where asEmacsValue (EmacsKeyword ev) = ev
instance AsEmacsValue EmacsCons     where asEmacsValue (EmacsCons ev) = ev
instance AsEmacsValue EmacsList     where asEmacsValue (EmacsList ev) = ev
instance AsEmacsValue EmacsFunction where asEmacsValue (EmacsFunction ev) = ev

-- Symbol
class ToEmacsValue s => ToEmacsSymbol s where
  toEmacsSymbol :: (MonadIO m, HasEmacsCtx m) => s -> m EmacsSymbol
instance ToEmacsSymbol EmacsSymbol where
  toEmacsSymbol = pure
instance ToEmacsSymbol Symbol      where
  toEmacsSymbol (Symbol t) = EmacsSymbol <$> intern t

-- Keyword
class ToEmacsValue s => ToEmacsKeyword s where
  toEmacsKeyword :: (MonadIO m, HasEmacsCtx m) => s -> m EmacsKeyword
instance ToEmacsKeyword EmacsKeyword where
  toEmacsKeyword = pure
instance ToEmacsKeyword Keyword where
  toEmacsKeyword (Keyword t) = EmacsKeyword <$> intern (":" <> t)

-- Cons
class ToEmacsValue s => ToEmacsCons s where
  toEmacsCons :: (MonadIO m, HasEmacsCtx m) => s -> m EmacsCons
instance ToEmacsCons EmacsCons where
  toEmacsCons = pure
instance (ToEmacsValue a, ToEmacsValue b) => ToEmacsCons (a, b) where
  toEmacsCons (a,b) = do
    av <- toEv a
    bv <- toEv b
    mkCons av bv

-- List
class ToEmacsValue s => ToEmacsList s where
  toEmacsList :: (MonadIO m, HasEmacsCtx m) => s -> m EmacsList
instance ToEmacsList EmacsList where
  toEmacsList = pure
instance ToEmacsValue x => ToEmacsList [x] where
  toEmacsList xs = EmacsList <$> (mkList =<< mapM toEv xs)

class (Callable s,ToEmacsValue s) => ToEmacsFunction s where
  toEmacsFunction :: (MonadIO m, HasEmacsCtx m) => s -> m EmacsFunction

instance ToEmacsFunction EmacsFunction where
  toEmacsFunction = pure

instance (FromEmacsValue a, Callable b) => ToEmacsFunction (a -> b) where
  toEmacsFunction f = EmacsFunction <$> mkFunctionFromCallable f

-- Function call Utilities
funcall1
  :: (MonadIO m, HasEmacsCtx m, ToEmacsValue a)
  => Text
  -> a
  -> m EmacsValue
funcall1 fname ev0 =
  join $ funcall <$> intern fname
                 <*> sequence [toEv ev0]

funcall2
  :: (MonadIO m, HasEmacsCtx m, ToEmacsValue a, ToEmacsValue b)
  => Text
  -> a
  -> b
  -> m EmacsValue
funcall2 fname ev0 ev1 =
  join $ funcall <$> intern fname
                 <*> sequence [toEv ev0, toEv ev1]

funcall3
  :: (ToEmacsValue a, ToEmacsValue b, ToEmacsValue c)
  => Text
  -> a
  -> b
  -> c
  -> EmacsM EmacsValue
funcall3 fname ev0 ev1 ev2 =
  join $ funcall <$> intern fname
                 <*> sequence [toEv ev0, toEv ev1, toEv ev2]


class FromEmacsValue h where
  fromEv :: EmacsValue -> EmacsM h

instance FromEmacsValue Int where
  fromEv = extractInteger

instance FromEmacsValue Text where
  fromEv = extractString

instance FromEmacsValue EmacsValue where
  fromEv = pure

instance FromEmacsValue EmacsFunction where
  fromEv = pure . EmacsFunction

class Callable a where
    call :: a -> [EmacsValue] -> EmacsM (Either Text EmacsValue)
    arity :: a -> Int

instance {-# OVERLAPPING #-} ToEmacsValue a => Callable a where
    call a [] = Right <$> toEv a
    call _ _  = pure $ Left "Too many arguments"
    arity _ = 0

instance {-# OVERLAPPING #-} ToEmacsValue a => Callable (IO a) where
    call a [] = do
      v <- liftIO a
      Right <$> toEv v
    call _ _  = pure $ Left "Too many arguments"
    arity _ = 0

instance {-# OVERLAPPING #-} ToEmacsValue a => Callable (EmacsM a) where
    call am [] = do
      a <- am
      Right <$> toEv a
    call _ _  = pure $ Left "Too many arguments"
    arity _ = 0

instance {-# OVERLAPPING #-} (FromEmacsValue a, Callable b) => Callable (a -> b) where
  call f (e:es) = do
    av <- fromEv e
    call (f av) es
  call _ [] = pure $ Left "Too less arguments"
  arity f = arity (f (error "Callable (a -> b)")) + 1


mkFunctionFromCallable :: (MonadIO m, HasEmacsCtx m) => Callable f => f -> m EmacsValue
mkFunctionFromCallable f = do
  let a = arity f
  mkFunction func a a ""
  where
    func :: [EmacsValue] -> EmacsM EmacsValue
    func es = do
      res <- call f es
      case res of
        Right ev -> return ev
        Left e -> fail $ "mkFunctionFromCallable failed for f with " <> show (length es) <> " args: " <> show e

evalString :: Text -> EmacsM EmacsValue
evalString t =
  funcall1 "eval" =<< (car =<< funcall1 "read-from-string" t)

provide :: Text -> EmacsM ()
provide feature =
  void $ funcall1 "provide" (Symbol feature)

message :: Text -> EmacsM ()
message t =
  void $ funcall1 "message" t

print :: ToEmacsValue v => v -> EmacsM ()
print ev =
  void $ funcall1 "print" ev

mkCons
  :: (MonadIO m, HasEmacsCtx m, ToEmacsValue a, ToEmacsValue b)
  => a
  -> b
  -> m EmacsCons
mkCons a b =
  EmacsCons <$> funcall2 "cons" a b

car :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> m EmacsValue
car = funcall1 "car"

cdr :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> m EmacsValue
cdr = funcall1 "cdr"
