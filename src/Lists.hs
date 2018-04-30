module Lists where

-- | 
-- Problem 1: Find the last element of a list
--
myLast :: [a] -> a
myLast [] = error "Empty list has no last element"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

-- |
-- Problem 2: Find the last but one element of a list
--
myButLast :: [a] -> a
myButLast [] = error "Empty list has no elements"
myButLast (x:[]) = error "Empty list has no elements"
myButLast (x1:x2:[]) = x1
myButLast (x:xs) = myButLast xs

-- | 
-- Problem 3: Find the kth element of a list.  The first element in the list is
-- number 1
--
elementAt :: [a] -> Int -> a
elementAt l n 
  | n <= 0 = error "Invalid index"
  | otherwise = impl l 1
  where
    impl :: [a] -> Int -> a
    impl [] _ = error "Empty list"
    impl (x:xs) curr = if curr == n then x else impl xs (curr + 1)

-- | 
-- Problem 4: Find the number of elements in a list
--
myLength :: [a] -> Int
myLength [] = 0
myLength (x:[]) = 1
myLength (x:xs) = 1 + myLength xs

-- | 
-- Problem 5: Reverse a list
--
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- | 
-- Problem 6: Find out whether a list is a palindrome. A palindrome can be read 
-- forward or backward; e.g. (x a m a x).
--
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == myReverse l

-- |
-- Problem 7: Transform a list, possibly holding lists as elements into a `flat' list by 
-- replacing each list with its elements (recursively).
--
data NestedList a = Elem a | List [NestedList a] 

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ (flatten (List xs))

-- |
-- Problem 8: If a list contains repeated elements they should be replaced with 
-- a single copy of the element. The order of the elements should not be changed.
--
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x1:x2:xs) = if x1 == x2 
  then compress $ x1 : xs 
  else x1 : (compress (x2 : xs))

-- |
-- Problem 9: Pack consecutive duplicates of list elements into sublists. If a 
-- list contains repeated elements they should be placed in separate sublists
--
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = impl [] x [x] xs
  where
    impl :: Eq a => [[a]] -> a -> [a] -> [a] -> [[a]]
    impl answer _currValue currList [] = answer ++ [currList]
    impl answer currValue currList (y:ys) = if y == currValue
      then impl answer currValue (y:currList) ys
      else case ys of
        [] -> answer ++ [currList] ++ [[y]]
        remaining -> impl (answer ++ [currList]) y [y] remaining

-- | 
-- Problem 10: Run-length encoding of a list. Use the result of problem P09 to 
-- implement the so-called run-length encoding data compression method. 
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the 
-- number of duplicates of the element E
--
encode :: Eq a => [a] -> [(Int, a)]
encode l = map f (pack l)
  where
    f [] = error "We will not hit an empty list"
    f (x:xs) = (1 + myLength xs, x)
