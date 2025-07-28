{-# OPTIONS_GHC -fno-warn-orphans #-}
module Emacs.Type.ToEmacsListInstances where

import Emacs.Type
    ( ToEmacsList(..),
      ToEmacsValue(..),
      AsEmacsValue(asEmacsValue),
      EmacsList(..) )
import Emacs.Prelude
import Emacs.Internal.List ( mkList )

instance ToEmacsValue h => ToEmacsValue [h] where
  toEv = (asEmacsValue<$>) . toEmacsList
instance ToEmacsList EmacsList where
  toEmacsList = pure
instance ToEmacsValue x => ToEmacsList [x] where
  toEmacsList xs = EmacsList <$> (mkList =<< mapM toEv xs)
