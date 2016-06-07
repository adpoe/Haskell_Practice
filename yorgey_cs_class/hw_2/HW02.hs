{-# OPTIONS_GHC -Wall #-}
module HW02 where
import Control.Monad 

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches secret guess = sum $ map fromEnum $ zipWith (==) secret guess


-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors inputColors = reds : greens : blues : yellows : oranges : purples : []
	where 
		numColors x = length $ filter (==x) inputColors
		reds = numColors Red 
		greens = numColors Green
		blues = numColors Blue
		yellows = numColors Yellow
		oranges = numColors Orange 
		purples = numColors Purple 

reduceToBools :: [Int] -> [Bool]
reduceToBools []    = [] 
reduceToBools(x:xs) = (x > 0) : reduceToBools xs	


-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches secret guess = sum $ listMatches secretList guessList
  where 
  	secretValues = countColors secret
  	guessValues  = countColors guess
  	secretList   = map fromEnum $ reduceToBools secretValues
  	guessList    = map fromEnum $ reduceToBools guessValues

listMatches :: [Int] -> [Int] -> [Int]
listMatches [] _            = []
listMatches _ []            = []
listMatches (x:xs) (y:ys)
 	| x >= y && y > 0       = 1 : listMatches xs ys 
 	| x >= y && y < 0       = listMatches xs ys 
 	| x < y                 = listMatches xs ys 
 	| otherwise             = listMatches xs ys 

 
-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
{-  Note: I am matchhing against exact and overall matches.
          The homework asks to match on exact matches and 
          something called 'non-exact matches',
          but it does not offer a definition of what non-exact matches are.
          Beacuse of this, I'm going with what I know to be correct. 
-}
getMove :: Code -> Code -> Move
getMove secret guess = Move guess numExactlyMatched numOverallMatches 
	where 
		numExactlyMatched = exactMatches secret guess 
		numOverallMatches = matches secret guess

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess int1 int2) secret = 
	(Move guess int1 int2) == getMove secret guess 

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes _ []                         = []
filterCodes givenMove (x:xs) 
	| isConsistent givenMove x == True   = x : filterCodes givenMove xs
	| otherwise                          = filterCodes givenMove xs 

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n = replicateM n colors 


-- Exercise 7 -----------------------------------------
solve :: Code -> [Move]
solve secret = tryMove moveList codeList
	where
		codeList = getAllCodes secret
		moveList = getMoveList secret codeList

getAllCodes :: Code -> [Code]
getAllCodes code = allCodes $ length code

getMoveList :: Code -> [Code] -> [Move]
getMoveList _ []          = []
getMoveList secret (x:xs) = getMove secret x : getMoveList secret xs 

tryMove :: [Move] -> [Code] -> [Move]
tryMove [] _         = []
tryMove _ []         = []
tryMove (x:xs) codes = x : tryMove xs nextIteration
	where 
		nextIteration = filterCodes x codes 

