-- https://wiki.haskell.org/99_questions/1_to_10

-- 1. Find the last element of a list
myLast :: [a] -> a
myLast [] = error "No end for an empty list"
myLast [a] = a
myLast (_:xs) = myLast xs

myLast' = head . reverse


-- 2. Find the last but one element of a list
myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- 3. Find the K'th element of a list. The first element in the list is number 1
elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list"
elementAt list num = list !! (num - 1)

-- 4. Find the number of elements of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 5. Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- 6. Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x)
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = list == rlist
	where rlist = reverse list

-- 7. Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x