module Emacs.Prelude
  ( module X
  , NEL
  ) where


import Control.Lens as X ((^.), (%~), _Left, _Right)
import Data.Functor.Identity as X
import Emacs.Pretty as X
import Refined as X
import Relude as X hiding (NonEmpty, Predicate, atomically, lookupEnv)
import Relude qualified as R

type NEL = R.NonEmpty
