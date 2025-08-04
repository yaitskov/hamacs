module Emacs.Test where

import Emacs.Prelude ( Applicative(pure), IO, catch, throwIO )
import System.Exit ( ExitCode(ExitSuccess) )
import Test.Tasty ( defaultMain, TestTree )

runTestsWithoutExit :: TestTree -> IO ()
runTestsWithoutExit tt =
  defaultMain tt `catch` (\case ExitSuccess -> pure () ; o -> throwIO o)
