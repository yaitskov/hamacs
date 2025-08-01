module Emacs.Internal (
    module Emacs.Type,
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
    mkFunction,
    mkInteger,
    mkString,
    intern,
    intern',
    mkList,
    mkNil,
    mkT,
    --
    funcall,
    errorHandle,
    checkExitStatus
    ) where

import Emacs.Prelude hiding (typeOf)
import Emacs.Type
import Emacs.Type.CallableInstances ()
import Emacs.Type.FromEmacsValueInstances ()
import Emacs.Type.ToEmacsKeywordInstances ()
import Emacs.Type.ToEmacsListInstances ()
import Emacs.Type.ToEmacsSymbolInstances ()
import Emacs.Type.ToEmacsValueInstances ()
import Emacs.Internal.Eq ( eq )
import Emacs.Internal.Check ( checkExitStatus, nonLocalExitThrow )
import Emacs.Internal.Intern ( intern, intern' )
import Emacs.Internal.Funcall ( funcall )
import Emacs.Internal.Function ( mkFunction, errorHandle, mkList )
import Emacs.Internal.String ( mkString, extractString )
import Emacs.Internal.Integer ( mkInteger, extractInteger )
--import Emacs.Internal.List ( mkList )
import Emacs.Internal.Nil ( mkNil, isNotNil, isNil )
import Emacs.Internal.T ( mkT )
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
