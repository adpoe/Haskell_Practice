import Data.Char

replicate' n a = [True | _ <- [1 .. n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1 .. n], y <- [1 .. n ], z <- [1 .. n], x ^ 2 + y ^ 2 == z ^ 2]

find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0 .. n])
	where n = length xs - 1

scalarproduct :: [ Int ] -> [ Int ] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- xs `zip` ys]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c 
	| isLower c = int2let ((let2int c + n) `mod` 26)
	| isUpper c = int2let ((let2int c + n) `mod` 26)
	| otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

divisors :: Int -> [Int]
divisors x = [d | d <- [1 .. x], x `divides` d]



divides :: Int -> Int -> Bool
divides x y 
	| x `mod` y == 0 = True
	| otherwise = False 