module Emacs.Internal (
    module Emacs.Type,
    module X,
    nonLocalExitThrow,
    mkGlobalRef,
    getEmacsEnvFromRT,
    -- Type relaties one..
    typeOf,
    isTypeOf,
    -- emacs -> haskell
    extractInteger,
    extractString,
    --
    eq,
    isNotNil,
    isNil,
    -- mk
    mkInteger,
    mkString,
    intern,
    intern',
    T(T), Nil(Nil),
    mkNil,
    mkT,
    --
    funcall,
    checkExitStatus
    ) where

import Emacs.Prelude hiding (typeOf)
import Emacs.Type
import Emacs.Type.CallableInstances ()
import Emacs.Type.FromEmacsValueInstances ()
import Emacs.Type.ToEmacsKeywordInstances ()
import Emacs.Type.ToEmacsSymbolInstances ()
import Emacs.Type.ToEmacsValueInstances ()
import Emacs.Internal.Eq ( eq )
import Emacs.Internal.Check ( checkExitStatus, nonLocalExitThrow )
import Emacs.Internal.Intern ( intern, intern' )
import Emacs.Internal.Funcall ( funcall )
import Emacs.Internal.Function as X hiding (_make_function)
import Emacs.Internal.String ( mkString, extractString )
import Emacs.Internal.Integer ( mkInteger, extractInteger )
import Emacs.Internal.Nil ( mkNil, isNotNil, isNil, Nil(Nil) )
import Emacs.Internal.T ( mkT, T(T) )
import Emacs.Internal.TypeOf ( typeOf, isTypeOf )
import GHC.Ptr ( Ptr )

foreign import ccall _get_emacs_env_from_rt
  :: Ptr ()
  -> IO EmacsEnv

getEmacsEnvFromRT :: Ptr () -> IO EmacsEnv
getEmacsEnvFromRT =
  _get_emacs_env_from_rt

foreign import ccall _make_global_ref
  :: EmacsEnv
  -> EmacsValue
  -> IO GlobalEmacsValue

mkGlobalRef :: (MonadIO m, HasEmacsCtx m) => EmacsValue -> m GlobalEmacsValue
mkGlobalRef ev = do
  env <- getEmacsCtx
  checkExitStatus $ liftIO (_make_global_ref env ev)
