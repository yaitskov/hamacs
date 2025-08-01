module Emacs.Prelude
  ( module X
  , MonadUnliftIO
  , NEL
  ) where


import Control.Exception as X (AssertionFailed (..))
import Control.Lens as X ((^.), (%~), (.~), _Left, _Right, findMOf, each)
import Data.Data as X
import Data.Functor.Identity as X
import Data.Generics.Labels as X ()
import Emacs.Pretty as X
import Refined as X
import Relude as X hiding (NonEmpty, Predicate, atomically, lookupEnv)
import Relude qualified as R
import UnliftIO (MonadUnliftIO) -- qualified as X
import UnliftIO.Exception as X (throwIO, catch) -- qualified as X

type NEL = R.NonEmpty
