-- https://wiki.haskell.org/99_questions/1_to_10
module List1 where
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

	-- 8. Eliminate consecutive duplicates of list elements.
	compress :: Eq a => [a] -> [a]
	compress list = foldl foldingFunction [] list where
		foldingFunction [] el = [el]
		foldingFunction l el  = if last l == el then l else l ++ [el]

	-- 9. Pack consecutive duplicates of list elements into sublists. 
	-- If a list contains repeated elements they should be placed in separate sublists.
	pack :: (Eq a) => [a] -> [[a]]
	pack = foldr func [] where 
        func el [] = [[el]]
        func el (y:xs) =
            if el == (head y) then ((el:y):xs) else ([el]:y:xs)

    -- 10. Run-length encoding of a list
	encode :: [Char] -> [(Int, Char)]
	encode = foldl func [] where 
		func [] el = [(1, el)]
		func list el
			| snd (last list) == el = init list ++ [(fst (last list) + 1, el)]
			| otherwise             = list ++ [(1, el)]