module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Help (length, reverse, snoc)

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex fn = go 0
  where
  go :: Int -> List a -> Maybe Int
  go n (x : xs) | fn x = Just n
                | otherwise = go (n + 1) xs
  go _ Nil = Nothing

findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex fn xs = map ((length xs - 1) - _) (findIndex fn (reverse xs))

zip :: forall a b. List a -> List b -> List (Tuple a b)
zip xs ys = reverse (go xs ys Nil)
  where
  go Nil _ list = list
  go _ Nil list = list
  go (a : as) (b : bs) list = go as bs (Tuple a b : list )

unzipNew :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzipNew list = go list Nil Nil where
  go Nil list1 list2 = Tuple (list1) (list2)
  go (el : ls) Nil Nil = go ls ((fst el):Nil) ((snd el):Nil)
  go (el : ls) list1 list2 = go ls (snoc list1 (fst el)) (snoc list2 (snd el))

filter :: forall a. (a -> Boolean) -> List a -> List a
filter predicate = go Nil
  where
  go list Nil = reverse list
  go list (x : xs)
    | predicate x = go (x : list) xs
    | otherwise = go list xs

filterOpt :: forall a. (a -> Boolean) -> List a -> List a
filterOpt p = go Nil
  where
  go list Nil = reverse list
  go list (x : xs)
    | p x = go (x : list ) xs
    | otherwise = go list xs

take :: forall a. Int -> List a -> List a
take = go Nil
  where
  go list n _ | n < 1 = reverse list
  go list _ Nil = reverse list
  go list n (x : xs) = go (x : list ) (n - 1) xs

takeOpt :: forall a. Int -> List a -> List a
takeOpt = go Nil
  where
  go list n _ | n < 1 = reverse list
  go list _ Nil = reverse list
  go list n (x : xs) = go (x : list ) (n - 1) xs

testList :: List Int
testList = 3 : 10 : 22 : 0 : 4 : 2 : 1 : Nil

testList2 :: List Int
testList2 = 3 : 1 : 1 : 4 : 6 : 7 : 9 : 10 : Nil

main :: Effect Unit
main = do
  log ("Task 1 " <> show (findIndex (\n -> n > 2) testList))
  log ("Task 2 " <> show (findLastIndex (\n -> n > 2) testList))
  log ("Task 3 " <> show (zip testList2 testList))
  log ("Task 4 " <> show (unzipNew (zip testList2 testList)))
  log ("Task 5 " <> show (filter (\n -> n > 2) testList))
  log ("Task 6 Not Done")
  log ("Task 7 " <> show (take 2 testList))
  log ("Task 8 Not Done")


