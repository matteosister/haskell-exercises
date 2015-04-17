module List3 where
	
	-- 21
	insertAt :: a -> [a] -> Int -> [a]
	insertAt _ _ 0 = error "0 is not a valid position"
	insertAt elem list pos = fst split ++ [elem] ++ snd split
		where split = splitAt (pos - 1) list

	range :: Int -> Int -> [Int]
	range from to = [from..to]