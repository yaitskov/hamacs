module HaPack where

import Emacs --  (mkFunctionFromCallable, message, funcall2)
import Relude
import Test.Tasty as TT
import Test.Tasty.HUnit

fooBar :: Int -> Int
fooBar n = n + 33

fooBar0 :: Int
fooBar0 = 3333

sayHelloInEmacs :: Text -> EmacsM ()
sayHelloInEmacs msg = void $ funcall2 "message" ("Interpolate here: %s" :: Text) msg

{-# ANN sayHello Interactive #-}
sayHello :: EmacsM ()
sayHello = putStrLn "Hello from HINT"

third_function :: Int -> Int -> Int
third_function a b = a + b

iAmEmacsCompatibleFun :: Text -> EmacsM ()
iAmEmacsCompatibleFun txt = void $ message txt

($$$$) :: Int -> Int -> Int
($$$$) = (+)

sayHelloFromSaveExcursion :: EmacsM ()
sayHelloFromSaveExcursion = saveExcursion sayHello

fooBar0FromSaveExcursion :: EmacsM Int
fooBar0FromSaveExcursion = saveExcursion (pure fooBar0)

runHamacsApiTests :: EmacsM ()
runHamacsApiTests = withRunInIO $ \run -> defaultMain (tests run)
  where
    retOK = pure ("OK" :: Text)
    testCommandP run e fun =
      testCase (fun <> " is " <> show e) $
            (e  @=?) =<< run (commandp =<< intern (toText fun))

    tests (run :: (forall a. EmacsM a -> IO a)) = testGroup "Hamacs API"
      [ testGroup "commandp"
        [ testCommandP run True "set-mark-command"
        , testCommandP run False "message"
        ]
      , testGroup "save-excursion"
        [ testCase "ret OK from HS" $ ("OK" @=?) =<< run (saveExcursion retOK)
        , testCase "ret int from point" $ (1 @=?) =<< run (saveExcursion point)
        , testCase "recursive HS" $ ("OK" @=?) =<< run (saveExcursion (saveExcursion retOK))
        , testCase "recursive 3 HS" $ ("OK" @=?) =<< run (saveExcursion
                                                          (saveExcursion
                                                           (saveExcursion retOK)))
        ]
      ]
