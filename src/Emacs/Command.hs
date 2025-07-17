{-# LANGUAGE OverloadedStrings #-}
module Emacs.Command
 ( setCommand
 , defcommand'
 ) where

import Data.Text
import Emacs.Core
import Emacs.Prelude

setCommand :: Text -> InteractiveForm -> EmacsValue -> EmacsM ()
setCommand fname _form f = do
  fnameQ <- intern fname
  interactiveFormQ <- intern "interactive-form"
  void $ funcall2 "fset" fnameQ f
  void $ funcall3 "put"  fnameQ interactiveFormQ =<< evalString "'(interactive nil)"

defcommand'
  :: Text
  -> Doc
  -> InteractiveForm
  -> Arity
  -> ([EmacsValue] -> EmacsM EmacsValue)
  -> EmacsM ()
defcommand' fname (Doc doc) form (Arity comArity) f =
  setCommand fname form =<< mkFunction f comArity comArity doc
