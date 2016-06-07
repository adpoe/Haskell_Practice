{- Use a local variable - let ... in ... -}
strLength :: String -> Int
strLength []     = 0
strLength (_:xs) = let len_rest = strLength xs in
                   len_rest + 1

strLength' :: String -> Int
strLength' []     = 0
strLength' (_:xs) = 1 + strLength xs


{- define a local variable over multiple guarded branches, using where-}
frob :: String -> Char
frob [] = 'a'   -- leng is NOT in scope here
frob str
	| len > 5   = 'x'
	| len < 3   = 'y'
	| otherwise = 'z'
	where
		len = strLength str

-- Note:  Don't use tabs \t in Haskell source code

-- Use accumulators when necessary
sumTo20 :: [Int] -> Int
sumTo20 nums = go 0 nums -- the acc. starts at 0
  where go :: Int -> [Int] -> Int
        go acc [] = acc   -- empty list: return the accumulated sum
        go acc (x:xs)
           | acc >= 20 = acc 
           | otherwise = go (acc + x) xs


{- Use polymorphism carefully
 	All haskell functions must be parametric in their type parameters
 	This means they must not care of make decisions based on the choices
 	for those parameters. 
 	Can't do one thing whena is Int and a different thing when a is a Bool
 	No overloading, like in Java
 	Haskell uses type erasure at runtime to give a speed boost
-}
notEmpty :: [a] -> Bool
notEmpty (_:_)  = True
notEmpty []     = False

{- Haskell has both total and *partial* functions 
 	Partial functions WILL crash given some inputs, and that is expected
 	They can't work on EVERYTHING.
 	Better to avoid partial functions when possible. 
 	Some examples:  head, tail, init, last and (!!). All in Prelude.
 	These are a mistake. Don't use them. 
 	Instead, replace partial functions using pattern matching, when possible-}

doStuff1 :: [Int] -> Int
doStuff1 []   = 0
doStuff1 [_]  = 0
doStuff1 xs   = head xs + (head(tail xs))

-- Replace above funtion with this one, using pattern matching
doStuff2 :: [Int] -> Int
doStuff2 []        = 0
doStuff2 [_]       = 0
doStuff2 (x1:x2:_) = x1 +x2

-- Recursion patterns
-- MAP
addOneToAll :: [Int] -> [Int]
addOneToAll []     = []
addOneToAll (x:xs) = x + 1 : addOneToAll xs

absAll :: [Int] -> [Int]
absAll []      = []
absAll (x:xs)  = abs x : absAll xs

squareAll :: [Int] -> [Int]
squareAll []      = []
squareAll (x:xs)  = x^2 : squareAll xs

map :: (a -> b) -> [a] -> [b]
map _ []       = []
map f (x:xs)   = f x : map f xs

-- FILTER
keepOnlyPositive :: [Int] -> [Int]
keepOnlyPositive [] = []
keepOnlyPositive (x:xs)
  | x > 0       = x : keepOnlyPositive xs
  | otherwise   = keepOnlyPositive xs

keepOnlyEven :: [Int] -> [Int]
keepOnlyEven [] = []
keepOnlyEven (x:xs)
  | even x      = x : keepOnlyEven xs
  | otherwise   = keepOnlyEven xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x        = x : filter p xs
  | otherwise = filter p xs 

-- FOLD
-- Combine elements of the list in a sort of final answer  
sum' :: [Int] -> [Int]
sum' []       = 0
sum' (x:xs)   = x + sum' xs

product' :: [Int] -> Int
product' [] = 1
product' (x:xs)  = x * product' xs

length' :: [a] -> Int
length' []      = 0
length' (_:xs)  = 1 + length' xs 

fold :: (a -> b -> b) -> b -> [a] -> b 
fold f z []      = z
fold f z (x:xs)  = f x (fold f z xs) 

-- note, in general want to use foldl' from Data.List 

-- FUNCTIONAL PROGRAMMING
-- Use (.) function for composition
add1Mul4 :: [Int] -> [Int]
add1Mul4 x = map ((*4) . (+1)) x 

negateNumEvens1 :: [Int] -> Int
negateNumEvens1 x = negate (length (filter even x))

-- Use ($) to get rid of parentheses
negateNumEvens2 :: [Int] -> [Int]
negateNumEvens2 x = negate $ length $ filter even x

-- Lambdas
-- Use as anonymous functions which are trivial and don't need a name
duplicate1 :: [String] -> [String]
duplicate1 = map dup 
	where dup x = x ++ x 

duplicate2 :: [String] -> [String]
duplicate2 = map (\x -> x ++ x)

-- CURRYING and PARTIAL APPLICATION
f :: Int -> Int -> Int 
f x y = 2*x + y

f' :: Int -> (Int -> Int)

f'' :: (Int, Int) -> Int
f'' (x,y) = 2*x + y

schonfinkel :: ((a,b) -> c) -> a -> b -> c
schonfinkel f x y = f (x,y)

unschonfinkel :: (a -> b -> c) -> (a,b) -> c
unschonfinkel f (x,y) = f x y 

foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
  | x > 3      = (7*x + 2) + foobar xs 
  | otherwise foobar xs 

foobar' :: [Integer] -> Integer
foobar' = sum . map ((+2) . (*7)) . filter (>3)  




