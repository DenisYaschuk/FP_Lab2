module Help where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (fromMaybe, Maybe(..))

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

reverse :: forall a. List a -> List a
reverse = go Nil
  where
  go acc Nil = acc
  go acc (x : xs) = go (x : acc) xs

snoc :: forall a. List a -> a -> List a
snoc list el = reverse (el : reverse list)

tail :: forall a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : list) = Just list

length :: forall a. List a -> Int
length list =
  if null list
    then 0
    else 1 + (length ( fromMaybe Nil (tail list)))

head :: List ~> Maybe
head Nil = Nothing
head (x : _) = Just x