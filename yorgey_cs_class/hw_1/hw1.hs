-- PENN HASKELL CLASS - HOMEWORK 1

-- | Homework 1 for Yorgey Class
--   http://www.cis.upenn.edu/~cis194/hw/01-intro.pdf


-- EXERCISES

-- Exercise 1 - Break the card number into a list of single digits. 
toDigits :: Integer -> [Integer]
toDigits n 
	| n <= 0                              = [ ]
	| n `div` 10 == 0 && n `mod` 10 == n  = [n]
	| otherwise                           = reverse $ n `mod` 10 : toDigitsRev (n `div` 10) 


toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
	| n <= 0                              = [ ]
	| n `div` 10 == 0 && n `mod` 10 == n  = [n]
	| otherwise                           =  n `mod` 10 : toDigitsRev (n `div` 10) 


-- Exercise 2 - Once we have the digits in the proper order, we need to double every other one.

doubleEveryOtherForward :: [Integer] -> [Integer]
doubleEveryOtherForward [] = []
doubleEveryOtherForward [x] = [x]
doubleEveryOtherForward (x:y:zs) = x : (y*2) : doubleEveryOtherForward zs


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse . doubleEveryOtherForward . reverse $ n


-- Exercise 3 - The output of doubleEveryOther has a mix of one-digit and two-digit numbers. Define a function that calculates the sum of EACH digit. (i.e. - 123 is 1 + 2 + 3)
sumDigits :: [Integer] -> Integer
sumDigits d = foldr (+) 0 $ concat ( map toDigits d )


-- Exercise 4 - Define a function that indicates whenther an Integer could be a valid credit card number. This will use all functions from previous exercises. 
validate :: Integer -> Bool 
validate x 
	| validated == 0      = True
	| otherwise	          = False 
	where 
		flat = toDigits x
		dub = doubleEveryOther flat
		summed = sumDigits dub
		validated = summed `mod` 10


-- Exercise 5 - Towers of hanoi puzzle. 

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
	| n == 0    = []
	| otherwise = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a  

