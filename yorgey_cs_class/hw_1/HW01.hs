{-# OPTIONS_GHC -Wall #-}
module HW01 where

	{- ALGORITHM:
     1.  Double value of every second digit
     beginning on RIGHT
     i.e:  [5,5,9,4] -> [10, 5, 18, 4]
     2. Add the digits of the doubled values to the 
     undoubled digits from the original number.
     i.e.: [10, 5, 18, 4] -> (1+0) + 5 + (1 + 8) + 4 = 19
     3. Calculate the remainder when the sum is divided by 10.
     i.e.: for the example above, remainder is 9

     If the result equals 0, then the number is valid 
-}


-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit 0 = 0
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = fromDigits (init (toDigits n) )

toDigits :: Integer -> [Integer]
toDigits n 
	| n < 1 = []
	| otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

fromDigits :: [Integer] -> Integer
fromDigits = foldl addDigit 0
	where addDigit num d = 10*num + d 


-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n 
	| n < 1 = []
	| otherwise = n `mod` 10 : toRevDigits (n `div` 10)



-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []      = [ ]
doubleEveryOther [x]     = [x]           
doubleEveryOther(x:y:xs) = x : y * 2 : (doubleEveryOther xs)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (+) 0 $ concat ( map toDigits xs ) 


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn num
	| validated == 0    = True
	| otherwise         = False
	where 
		reversed = toRevDigits num
		doubled = doubleEveryOther reversed
		summed = sumDigits doubled 
		validated = summed `mod` 10

-- Exercise 6 -----------------------------------------

{- Towers of Hanoi Puzzle: 
     * Move all disks from first peg to last peg
     * Must move one disk at a time
     * A larger disk may never be stacked on top of a smaller one
      -}

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi num peg1 peg2 peg3 
	| num == 0  = []
	| otherwise = hanoi (num-1) peg1 peg3 peg2 ++ [(peg1, peg2)] ++ hanoi (num-1) peg3 peg2 peg1
