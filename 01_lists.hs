-- Find the last element of a list.

myLast :: [a] -> a
myLast [] = error "No end for an empty list"
myLast [a] = a
myLast (_:xs) = myLast xs

myLast' = head . reverse


-- Find the last but one element of a list.

myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- Find the K'th element of a list. The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list"
elementAt list num = list !! (num - 1)