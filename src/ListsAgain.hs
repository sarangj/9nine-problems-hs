module ListsAgain where

import qualified Lists

-- |
-- Problem 21: Insert an element at a given position into a list.
--
insertAt :: a -> [a] -> Int -> [a]
insertAt v [] n = if n == 1 then [v] else []
insertAt v l n = impl 1 l 
  where
    impl _ [] = []
    impl curr (y:ys) = 
      if curr == n then v:y:ys else y:(impl (curr+1) ys)

-- |
-- Problem 22: Create a list containing all integers within a given range.
--
range :: Int -> Int -> [Int]
range a b 
  | a == b = [a]
  | a < b = a : (range (a+1) b)
  | otherwise = []
