skips :: [a] -> [[a]]
skips []      = [] 
skips list    = [everyN nums list | nums <- (lengthList list)]
	where everyN n xs = case drop (n-1) xs of
		(y:ys) -> y : everyN n ys
		[] -> [] 


lengthList :: [a] -> [Int]
lengthList input = [1..(length input)]

