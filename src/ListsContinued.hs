module ListsContinued where

import qualified Lists

-- |
-- Problem 11: Modify the result of problem 10 in such a way that if an element 
-- has no duplicates it is simply copied into the result list. Only elements 
-- with duplicates are transferred as (N E) lists.
--
data EncodedElement a = Multiple Int a | Single a 

instance Show a => Show (EncodedElement a) where
  show (Single a) = "Single " ++ show a
  show (Multiple n a) = "Multiple " ++ show n ++ " " ++ show a

encodeModified :: Eq a => [a] -> [EncodedElement a]
encodeModified = map f . Lists.pack
  where
    f [] = error "We will not hit an empty list"
    f (x:[]) = Single x
    f (x:xs) = Multiple (1 + Lists.myLength xs) x

-- |
-- Problem 12: Given a run-length code list generated as specified in problem 
-- 11. Construct its uncompressed version.
--
decodeModified :: [EncodedElement a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decodeSingle x ++ decodeModified xs
  where
    decodeSingle (Single y) = [y]
    decodeSingle (Multiple n y) = map (\_ -> y) [1 .. n]

-- |
-- Problem 12: Implement the so-called run-length encoding data compression
-- method directly. I.e. don't explicitly create the sublists containing the
-- duplicates, as in problem 9, but only count them. As in problem P11, simplify
-- the result list by replacing the singleton lists (1 X) by X.
-- 
encodeDirect :: Eq a => [a] -> [EncodedElement a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeImpl 1 x [] xs
  where 
    encodeImpl 
      :: Eq a 
      => Int 
      -> a 
      -> [EncodedElement a] 
      -> [a] 
      -> [EncodedElement a]
    encodeImpl n currValue answer [] = answer ++ [toEncodedElem n currValue]
    encodeImpl n currValue answer (y:ys) = if y == currValue
      then encodeImpl (n+1) currValue answer ys
      else encodeImpl 1 y (answer ++ [toEncodedElem n currValue]) ys
    toEncodedElem :: Int -> a -> EncodedElement a
    toEncodedElem m v = if m == 1 then Single v else Multiple m v

-- |
-- Problem 14: Duplicate the elements of a list.
--
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- |
-- Problem 15: Replicate the elements of a list a given number of times
--
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = repliImpl x n ++ repli xs n
  where
    repliImpl :: a -> Int -> [a]
    repliImpl y 1 = [y]
    repliImpl y n  = y : repliImpl y (n-1)

-- | 
-- Problem 16: Drop every N'th element from a list.
--
data Direction = Up | Down

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery lst n = impl lst n Down 
  where
    impl :: [a] -> Int -> Direction -> [a]
    impl [] _ _ = []
    impl (y:ys) 1 Down = impl ys 1 Up
    impl (y:ys) m Up = if m == n 
      then impl ys m Down 
      else y : impl ys (m+1) Up
    impl (y:ys) m Down = y : impl ys (m-1) Down 

-- |
-- Problem 17: Split a list into two parts; the length of the first part is 
-- given.
--
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split (x:xs) n = impl 1 [x] xs 
  where 
    impl :: Int -> [a] -> [a] -> ([a], [a])
    impl _ h [] = (h, [])
    impl m h (t:ts)
      | m >= n = (h, (t:ts))
      | m < n = impl (m+1) (h ++ [t]) ts 

-- |
-- Problem 18: Extract a slice from a list.
--
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) i k = impl x xs 1
  where
    impl :: a -> [a] -> Int -> [a]
    impl y [] curr = if curr >= i && curr < k then [y] else []
    impl y (z:zs) curr 
      | curr == k = [y]
      | curr > k = []
      | curr >= i = y : (impl z zs (curr + 1))
      | otherwise = impl z zs $ curr + 1

-- |
-- Problem 19: Rotate a list N places to the left
--
rotate :: [a] -> Int -> [a]
rotate l n = end ++ start
  where
    (start, end) = split l n 

-- | 
-- Problem 20: Remove the K'th element from a list.
--
removeAt :: [a] -> Int -> (Maybe a, [a])
removeAt l n = case (split l (n - 1)) of 
  (strt, []) -> (Nothing, strt)
  (strt, (z:zs)) -> (Just z, strt ++ zs)

