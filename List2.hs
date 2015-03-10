module List2 where
	data Element a = Single a | Multiple Int a deriving (Show, Eq)
	--instance Show Element where
	--	show Single = "Single"
	--	show Multiple = "Multiple"

	encodeModified :: (Eq a) => [a] -> [Element a]
	encodeModified = foldr func [] where
		func newEl [] = [Single newEl]
		func newEl ((Single el):els)
			| newEl == el = ((Multiple 2 el):els)
			| otherwise   = ((Single newEl):(Single el):els)
		func newEl ((Multiple num el):els)
			| newEl == el = ((Multiple (num + 1) el):els) 
			| otherwise   = ((Single newEl):(Multiple num el):els)
