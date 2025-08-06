module Emacs.Command
 ( setCommand
 , defcommand'
 ) where

import Emacs.Core
import Emacs.Prelude ( ($), Text, void, (=<<) )


setCommand :: MonadEmacs m => Text -> InteractiveForm -> EmacsValue -> m ()
setCommand fname _form f = do
  fnameQ <- intern fname
  interactiveFormQ <- intern "interactive-form"
  void $ funcall2 "fset" fnameQ f
  void $ funcall3 "put"  fnameQ interactiveFormQ =<< evalString "'(interactive nil)"

defcommand'
  :: Text
  -> EmDocString
  -> InteractiveForm
  -> Arity
  -> ([EmacsValue] -> NativeEmacsM EmacsValue)
  -> NativeEmacsM ()
defcommand' fname eds form (Arity comArity) f =
  setCommand fname form =<< mkFunction f comArity comArity eds
