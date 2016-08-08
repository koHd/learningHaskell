-- 2016 January, 2015 Autumn

-- reverse with count
reverseWithCount :: [a] -> [Int] -> [a]
reverseWithCount [] _ = []
reverseWithCount (l:ls) (n:ns) =  reverseWithCount ls ns ++ copy l n

-- make n copies of a letter
copy :: a -> Int -> [a]
copy letter 0 = []
copy letter n = letter : copy letter (n-1)

-- 2015 January

{- afterFilter 
	input: predicate p, list xs
	returns: list of those elements of xs which immediately follow an element which passes the predicate p 
	examples: 
		>afterFilter(<0) [-4, 7, -4, -8, 3, -3, -6, 0, -9, 1]
		[7, -8, 3, -6, 0, -1]
		
		>afterFilter(=='f') "fifferfefferfather"
		"ifeefea"
		-}
		
--afterFilter :: Bool -> [a] -> [a]
afterFilter _ [] = []
afterFilter filter (x:xs)
	| filter x = (head xs) : afterFilter filter xs
	| otherwise = afterFilter filter xs

-- 2014 January

{- mapEveryOther
	input: function f, list xs
	returns: list with alternating elements of the function applied to element of xs and the raw element from xs
	examples:
		>mapEveryOther (+1000) [0,1,2,3,4,5,6]
		[1000,1,1002,3,1004,5,1006]
		-}
		
mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther _ [] = []
mapEveryOther f [x] = [f x]
mapEveryOther f (x:y:xs) = f x : y : mapEveryOther f xs