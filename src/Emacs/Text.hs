module Emacs.Text where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.List qualified as L
import Relude
    ( Eq((==)),
      Bool,
      Char,
      Word8,
      Text,
      (.),
      ByteString,
      LByteString,
      LText )

class StringApi s where
  type Elem s
  dropWhile :: (Elem s -> Bool) -> s -> s
  dropWhileEnd :: (Elem s -> Bool) -> s -> s

instance StringApi ByteString where
  type Elem ByteString = Word8
  dropWhile = BS.dropWhile
  dropWhileEnd = BS.dropWhileEnd

instance StringApi LByteString where
  type Elem LByteString = Word8
  dropWhile = BSL.dropWhile
  dropWhileEnd = BSL.dropWhileEnd

instance StringApi Text where
  type Elem Text = Char
  dropWhile = T.dropWhile
  dropWhileEnd = T.dropWhileEnd

instance StringApi LText where
  type Elem LText = Char
  dropWhile = TL.dropWhile
  dropWhileEnd = TL.dropWhileEnd

instance StringApi [a] where
  type Elem [a] = a
  dropWhile = L.dropWhile
  dropWhileEnd = L.dropWhileEnd

trimX :: (Eq (Elem a), StringApi a) => Elem a -> Elem a -> a -> a
trimX p s = dropWhile (== p) . dropWhileEnd (== s)
