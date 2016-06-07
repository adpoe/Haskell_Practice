-- Anonymous functions
gt100 :: Integer -> Bool
gt100 x = x > 100

greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter gt100 xs 

-- instead of creating a second function, can use a lambda
greaterThan100_2 :: [Integer] -> [Integer]
greaterThan100_2 xs = filter (\x -> x > 100) xs

-- (>100) is an operator section, partial application
greaterThan100_3 :: [Integer] -> [Integer]
greaterThan100_3 xs = filter (>100) xs 

-- Function composition
{-
(b -> c) -> (a -> b) -> (a -> c)
What function has this type? 
-}
foo :: (b -> c) -> (a -> b) -> (a -> c)
foo f g = \x -> f (g x)

myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100 

-- Currying and partial application
f :: Int -> Int -> Integer
f x y = 2*x + y

f' :: Int -> (Int -> Int)
f' x y = 2*x + y

comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)

f'' :: (Int, Int) -> Int
f'' (x,y) = 2*x + y

schonfinkel :: ((a,b) -> c) -> a -> b -> c
schonfinkel f x y = f (x,y)

unschonfinkel :: (a -> b -> c) -> (a,b) -> c
unschonfinkel f (x,y) - f x y

foobar :: [Integer] -> Integer
foobar []      = 0
foobar (x:xs)  
  | x > 3      = (7*x + 2) _ foobar xs
  | otherwise  = foobar xs

foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7*x +2 ) . filter (>3)

-- Folds
fold :: b -> (a -> b -> b) -> [a] -> b
fold z f []      = z
fold z f (x:xs)  = f x (fold z f xs)











