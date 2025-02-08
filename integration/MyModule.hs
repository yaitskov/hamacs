module MyModule where

-- import Prelude
-- import Control.Monad.IO.Class
-- import Protolude
import Relude
import Emacs
import Emacs.Type

callMeFromEmacs :: EmacsM ()
callMeFromEmacs = do
  -- pure ()
  afh <- accessFormHint <$> getPState
  -- liftIO $ putStrLn ("callMeFromEmacs: accessFormHint " <> show returnHello) -- afh)
  putStrLn ("callMeFromEmacs: accessFormHint" <> show afh)
