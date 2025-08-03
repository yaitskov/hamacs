module Emacs.Type.Annotations where

import Emacs.Prelude (Text, Eq, Data, Show )

data Interactive = Interactive deriving (Show, Eq, Data)

data Haddock = Haddock Text deriving (Show, Eq, Data)
