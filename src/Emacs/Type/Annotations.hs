module Emacs.Type.Annotations where

import Emacs.Prelude
    ( Eq, Data, Show, Semigroup, Monoid(mempty), Text )
data Interactive = Interactive deriving (Show, Eq, Data)

data Haddock = Haddock Text deriving (Show, Eq, Data)

newtype EmDocString = EmDocString { unEmDocString :: Text } deriving  (Show, Eq, Data) deriving newtype (Semigroup)

instance Monoid EmDocString where
  mempty = EmDocString ""
