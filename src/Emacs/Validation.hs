module Emacs.Validation where

import Emacs.Prelude

type family Columnar f r a where
  Columnar Identity r a = Refined r a
  Columnar Maybe _ a = Maybe a


refineS :: forall {k} (p :: k) x. Predicate p x => String -> x -> Either String (Refined p x)
refineS fieldName =
  (_Left %~ ((("Field [" <> fieldName <> "] is not valid due: ") <>) . show)) . refine

required :: forall {k} {p :: k} {a}. Predicate p a => String -> Maybe a -> Either String (Refined p a)
required s v =
  (maybe (Left $ "[" <> s <> "] field is required") pure v) >>= refineS s

type SeparatorValidator = And (SizeLessThan 5) NonEmpty
