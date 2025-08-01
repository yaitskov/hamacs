module HaPack where


-- import Emacs.Core
import Emacs --  (mkFunctionFromCallable, message, funcall2)
import Relude

fooBar :: Int -> Int
fooBar n = n + 33

fooBar0 :: Int
fooBar0 = 3333

sayHelloInEmacs :: Text -> EmacsM ()
sayHelloInEmacs msg = void $ funcall2 "message" ("Interpolate here: %s" :: Text) msg

sayHello :: EmacsM ()
sayHello = putStrLn "Hello from HINT"

third_function :: Int -> Int -> Int
third_function a b = a + b

iAmEmacsCompatibleFun :: Text -> EmacsM ()
iAmEmacsCompatibleFun txt = void $ message txt

($$$$) :: Int -> Int -> Int
($$$$) = (+)

sayHelloFromSaveExcursion :: EmacsM ()
sayHelloFromSaveExcursion = saveExcursion [sayHello]

fooBar0FromSaveExcursion :: EmacsM Int
fooBar0FromSaveExcursion = saveExcursion [pure fooBar0]
