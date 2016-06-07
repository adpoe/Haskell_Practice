biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

sumtorial ::  Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

foo :: Integer -> Integer
foo 0 = 16
foo 1 
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + 3

p :: (Int, Char)
p = (3, 'x') 

sumPair :: (Int, Int) -> Int 
sumPair (x, y) = x + y

f :: Int -> Int -> Int -> Int
f x y z = x + y + z

nums, range, range2 :: [Integer]
nums = [1,2,3,19]
range = [1..100]
range2 = [2,4..100]

-- hello1 and hello2 are exactly the same.
hello1 :: [Char]
hello1 = ['h', 'e', 'l', 'l', 'o']

hello2 :: String
hello2 = "hello"

helloSame = hello1 == hello2

ex18 = 1 : []
ex19 = 3 : (1 : [])
ex20 = 2 : 3 : 4 : []
ex21 = [2,3,4] == 2 : 3 : 4 : []

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

-- Generate the sequence of hailstone iterations from a starting number
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)


-- Compute the length of a list of Integers.
intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength(x:xs)  = 1 + intListLength xs 


sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []  -- Do nothing to the empty list
sumEveryTwo(x:[])      = [x] -- Do nothing to lists with a single element
sumEveryTwo(x:(y:zs))  = (x + y) : sumEveryTwo zs

-- The number of hailstone steps needed to reach 1 
-- from a starting number. 
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1
