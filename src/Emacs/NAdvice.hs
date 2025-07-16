{-# LANGUAGE OverloadedStrings #-}
module Emacs.NAdvice where


import Cases (spinalize)
-- import Protolude
import Relude
import Emacs.Core

-- Emacs 24 からアドバイスの機構が一新された(nadvice.el)。以前より大幅
-- にシンプルになっている。必要な関数は `advice-add` と
-- `advice-remove` の二つ。

-- (advice-add SYMBOL WHERE FUNCTION &optional PROPS)
--
-- Like ‘add-function’ but for the function named SYMBOL.
-- Contrary to ‘add-function’, this will properly handle the cases where SYMBOL
-- is defined as a macro, alias, command, ...
--
-- TODO: PROPSについては `add-function` のヘルプを参照

-- 基本的に Arround 一つで全て実装できる。
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

-- TODO: FUNCTION は シンボルでないと駄目？ -> いや、関数でもOK
-- 存在しないシンボルに対しても設定できる。
adviceAdd'
  :: (ToEmacsSymbol s, ToEmacsFunction f)
  => s
  -> Where
  -> f
  -> EmacsM ()
adviceAdd' target where' func =
  void $ funcall3 "advice-add" target (whereToKeyword where') func

-- 基本的にこの関数さえあれば何でもできる。
-- TODO: アドバイス外せるように
around' :: Callable f => Text -> (EmacsFunction -> f) -> EmacsM ()
around' name ff = do
  adviceAdd' (Symbol name) Around ff

-- aroundアドバイスの場合、大抵は引数は弄らない。ショートカット的。
-- TODO:
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
