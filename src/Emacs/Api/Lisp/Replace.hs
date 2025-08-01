module Emacs.Api.Lisp.Replace where

import Emacs.Api.Native.Buffer ( BufPos )
import Emacs.Api.Native.Search ( EmacsRegexp )
import Emacs.Core
import Emacs.Prelude ( Show, Generic, Maybe(Nothing), Text, (=<<) )


data ReplaceRegexp
  = ReplaceRegexp
  { delimited :: Maybe T
  , start :: Maybe BufPos
  , end :: Maybe BufPos
  , backward :: Maybe T
  , regionNoncontiguousP :: Maybe T
  } deriving (Show, Generic)

type Optional x = x -> x

replaceRegexp :: MonadEmacs m => EmacsRegexp -> Text -> Optional ReplaceRegexp -> m ()
replaceRegexp rx tos customize =
  fromEv =<< funcall7 "replace-regexp" rx tos o.delimited o.start o.end o.backward o.regionNoncontiguousP
  where
    o = customize
      ReplaceRegexp
      { delimited = Nothing
      , start = Nothing
      , end = Nothing
      , backward = Nothing
      , regionNoncontiguousP = Nothing
      }
