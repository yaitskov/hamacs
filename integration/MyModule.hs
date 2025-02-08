module MyModule where

-- import Prelude
-- import Control.Monad.IO.Class
-- import Protolude
import Relude
-- import Emacs
import Emacs.Type
import Data.Map.Strict as M

callMeFromEmacs :: EmacsM ()
callMeFromEmacs = do
  -- pure ()
  afh <- accessFormHint <$> getPState
  -- liftIO $ putStrLn ("callMeFromEmacs: accessFormHint " <> show returnHello) -- afh)
  putStrLn ("callMeFromEmacs: accessFormHint" <> show afh <> show (M.singleton True ()))
