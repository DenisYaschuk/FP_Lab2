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

filter :: forall arg. (arg -> Boolean) -> List arg -> List arg
filter predicate (el : lst) =
  if    predicate el
  then  el : (filter predicate lst)
  else  filter predicate lst
filter _ Nil = Nil

filterTR :: forall arg. (arg -> Boolean) -> List arg -> List arg
filterTR predicate list = tailRecursion predicate list Nil
  where
  tailRecursion :: (arg -> Boolean) -> List arg -> List arg -> List arg
  tailRecursion pred (el : lst) res = 
    if    pred el
    then  tailRecursion pred lst (el : res)
    else  tailRecursion pred lst res
  tailRecursion _ _ res = reverse res

take :: forall arg. Int -> List arg -> List arg
take num (el : lst) =
  if    num == 0
  then  Nil
  else  el : (take (num - 1) lst)
take _ _ = Nil

takeTR :: forall arg. Int -> List arg -> List arg
takeTR number list = tailRecursion number list Nil
  where
  tailRecursion :: Int -> List arg -> List arg -> List arg
  tailRecursion num (el : lst) res =
    if    num == 0
    then  reverse res
    else  tailRecursion (num - 1) lst ( el : res )
  tailRecursion _ _ res = reverse res

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
  log ("Task 6 " <> show (filterTR (\n -> n > 2) testList))
  log ("Task 7 " <> show (take 2 testList))
  log ("Task 8 " <> show (takeTR 2 testList))
