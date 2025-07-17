{-# LANGUAGE OverloadedStrings #-}
module Emacs.NAdvice where

import Cases (spinalize)
import Emacs.Core
import Relude

-- (advice-add SYMBOL WHERE FUNCTION &optional PROPS)
--
-- Like ‘add-function’ but for the function named SYMBOL.
-- Contrary to ‘add-function’, this will properly handle the cases where SYMBOL
-- is defined as a macro, alias, command, ...
--
--  `:before'       (lambda (&rest r) (apply FUNCTION r) (apply OLDFUN r))
--  `:after'        (lambda (&rest r) (prog1 (apply OLDFUN r) (apply FUNCTION r)))
--  `:around'       (lambda (&rest r) (apply FUNCTION OLDFUN r))
--  `:override'     (lambda (&rest r) (apply FUNCTION r))
--  `:before-while' (lambda (&rest r) (and (apply FUNCTION r) (apply OLDFUN r)))
--  `:before-until' (lambda (&rest r) (or  (apply FUNCTION r) (apply OLDFUN r)))
--  `:after-while'  (lambda (&rest r) (and (apply OLDFUN r) (apply FUNCTION r)))
--  `:after-until'  (lambda (&rest r) (or  (apply OLDFUN r) (apply FUNCTION r)))
--  `:filter-args'  (lambda (&rest r) (apply OLDFUN (funcall FUNCTION r)))
--  `:filter-return'(lambda (&rest r) (funcall FUNCTION (apply OLDFUN r)))
--
data Where
  = Around
  | Before
  | After
  | Override
  | BeforeWhile
  | BeforeUntil
  | AfterWhile
  | AfterUntil
  | FilterArgs
  | FIlterReturn
  deriving (Show, Eq, Ord, Enum)

whereToKeyword :: Where -> Keyword
whereToKeyword = Keyword . spinalize . show

-- whereToKeyword Before = Keyword "before"

adviceAdd'
  :: (ToEmacsSymbol s, ToEmacsFunction f)
  => s
  -> Where
  -> f
  -> EmacsM ()
adviceAdd' target where' func =
  void $ funcall3 "advice-add" target (whereToKeyword where') func

around' :: Callable f => Text -> (EmacsFunction -> f) -> EmacsM ()
around' name ff = do
  adviceAdd' (Symbol name) Around ff

around :: Callable f => Text -> (EmacsM EmacsValue -> f) -> EmacsM ()
around name ff = do
  adviceAdd' (Symbol name) Around =<< wrap ff
  where
    wrap :: Callable f => (EmacsM EmacsValue -> f) -> EmacsM EmacsFunction
    wrap newf =
      let wf :: [EmacsValue] -> EmacsM EmacsValue
          wf (func:args) = do
            res <- call (newf (funcall func args)) args
            case res of
              Right ev -> return ev
              -- TODO: convert EmacsValue to m String somehow
              Left  e -> fail $ "Funcall " <> show name <> " failed with " <> show e
          wf [] = fail $ "Empty list [" <> show name <> "]"
      in EmacsFunction <$> mkFunction wf 0 1000 "around advice"
