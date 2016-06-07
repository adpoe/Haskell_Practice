module Golf where
import Data.List

{- 
*****************************************
****  EXERCISE 1: HOPSCOTCH  ************
*****************************************

****************
** Score:  6 ***
****************

***  Define @skips@, which takes a list and ouputs a list of lists
***  First list is the same as input
***  Second list is every 2nd element from list
***  Third list is every 3rd element from list 
***  And so forth, with N being the Nth element ad infinitum 

			For example:
		skips "ABCD" == ["ABCD", "BD", "C", "D"]
		skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
		skips [1] == [[1]]
		skips [True,False] == [[True,False], [False]]
		skips [] == []
			::  Note that output should be same length as input
-}

skips :: [a] -> [[a]]
-- If list is empty, return [], empty list
skips []      = [] 
{- if list has data, create a list comprehennsion where:
	1.  We create a variable called "nums" 
	    which is a range of every number from 1 to the length of our input list
	2.  We run the @everyN@ function (described below) over:
	       a.   The list, for each number in the range
	            I.e., in a list of length 10, we run
				everyN 1 list
				everyN 2 list
				everyN 3 list
				..
				..
				everyN 10 list
-}
skips list    = [everyN nums list | nums <- (lengthList list)]
    {- Define an internal function @everyN@ which works by:
       1.  Feed function a number, and a list
       2.  Setup a case statement which:
             a.  Drops all elements prior to n from list
             b.  Gets the head of the remaining list (y)
                  :: (y) will be the nth element
             c.  Creates a NEW list, with y (nth element) as its head
             d.  Runs @everyN@ over the tail of the list, recursively 
             e.  This systematicall creates a list with only the "nth" elements
			 f.  Eventually list will be empty, and @everyN@ just returns a an empty list, ending the operation
		3.  Result is:  a list with all "n" elements, taken from "xs", the list passsed to @everyN@	 
             -}
	where everyN n xs = case drop (n-1) xs of
		(y:ys) -> y : everyN n ys
		[] -> [] 


lengthList :: [a] -> [Int]
lengthList input = [1..(length input)]


{-
*****************************************
****  EXERCISE 2:  LOCAL MAXIMA *********
*****************************************

*****************
*** Score: 6 ****
*****************
    But I needed help with this one.  Referenced this repo for help: 
    https://github.com/GrooveStomp/yorgey-haskell-course/blob/master/week-03/Golf.hs


****  A local maximum of a list is an element of the list which is strictly
****  greater than both the elements immediately before and after it. 

		For example, in the list [2,3,4,1,5], the only local maximum is 4, since
it is greater than the elements immediately before and after it (3 and
1). 5 is not a local maximum since there is no element that comes
after it.

		Write a function
		localMaxima :: [Integer] -> [Integer]
		which finds all the local maxima in the input list and returns them in
		order. For example:

		localMaxima [2,9,5,6,1] == [9,6]
		localMaxima [2,3,4,1,5] == [4]
		localMaxima [1,2,3,4,5] == []

-}


{- Function @everyThree@ gets a list of lists, containing every set of three from a given list of items
   Method is:
        1.  Create a list of numbers from 0 all the way to the length of the given list.
             ::  I.e., for a list of length 100 ([1..100]), the resulting list will contain all numbers 1 - 100.  
             ::  Then, drop three from that list, because the final three numbers will create a list with a length less than 3
        2.  Create lambda which:
             a.  Takes a number from the list created in step 1
             b.  Drops the given number of items from the list completely
             c.  Takes the next three numbers in sequence
             d.  Puts them in a list
        3.  Map that lambda over the full list of numbers created in step 1
             ::  This creates a list of lists, containing all permutations of 3, in sequence
-}
everyThree :: [Integer] -> [[Integer]]
everyThree list = map (\x -> take 3 (drop x list)) [0..length list - 3]


{- Function @compareThrees@ does the following: 
     1.   Gets the head of a list, compares against the 2nd element
     2.   Gets the last element of a list, again compares against 2nd element
     3.   If 2nd element is bigger than BOTH first and last elements, return True
     4.   Else, return False 
-}
compareThrees :: [Integer] -> Bool
compareThrees list = head list < head (tail list) && head (tail list) > last list  

{- Function @getMax@ takes a list, then runs @compareThrees@ on it
     1.  If the @compareThrees@ is True, return second element
     2.  If the @compareThrees@  is false, return empty list (cons) -}
getMax :: [Integer] -> [Integer]
getMax list 
	| compareThrees list = [list !! 1] 
	| otherwise          = []


{- @localMaxima does the following: 
	 1. Takes a list
	 2. Sets up a foldl, where:
	       a.  A lambda function pulls in the empty list as "x"
	       b.  The same lambda pulls in (everyThree Llist), as "y"
	            ::  (everyThree list) is a list of lists
	            ::  It serves as the list to fold over, for this function
	       c.  The lambda uses and empty list as the accumulator and:
	            ::  Applies @getMax@ to each item in (everyThree list), it's an [[Integer]]
	            ::  When @compareThrees@ within @getMax@ == True
	                  o The lambda pushes the max into into the accumulator
	            ::  The accumulator gets called as x again, this time with 1 more Integer
	            ::  And this process repeats
	            ::  The (++) inside the lambda continually adds the results of "getMax y" as an [Integer] -}
localMaxima :: [Integer] -> [Integer]
localMaxima list = foldl (\x y -> (++) x $ getMax y) [] $ everyThree list



{- 
*****************************************
*******  EXERCISE 3:  HISTOGRAM *********
*****************************************

Write a function:

histogram :: [Integer] -> String

which takes as input a list of Integers between 0 and 9 (inclusive),
and outputs a vertical histogram showing how many of each number
were in the input list. You may assume that the input list does not
contain any numbers less than zero or greater than 9 (that is, it does
not matter what your function does if the input does contain such
numbers).

See:  http://www.seas.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf for 
output specs + examples



-}

{- 
-- PROCEDURE

{-
	1.  Make 10 separate columns
	2.  Set up so that corresponding number add a newline 
	    +  Asterisk in the column
	3.  Then integrate into a whole. (intercalate?)

-}



testOne = [1,1,1,5]
testTwo = [1,4,5,4,6,6,3,4,2,4,9]

histBase = "\n==========\n0123456789\n"


histogram :: [Integer] -> String
histogram intList = testParse (groupNumbers intList) ++ histBase


groupNumbers :: [Integer] -> [Integer]
groupNumbers nums = sort nums 



testParse :: [Integer] -> [[Char]]
testParse [] = []
testParse (x:y:xs) 
	| x <= 10 && x >= 0  =  [[getSpaces x ++ "*"], testParse xs] 
	| otherwise          =  [] ++ testParse xs 


getSpaces :: Integer -> [Char]
getSpaces num = take (fromIntegral num)(repeat ' ')


-- removeExtraNewlines :: [Char] -> [Char]
-- removeExtraNewlines charString 


{-
parseArray :: [Integer] -> [Char]
parseArray [] = []
parseArray (x:xs) 
	| x == 0 = "\n*" ++ parseArray xs 
	| x == 1 = "\n *" ++ parseArray xs 
	| x == 2 = "\n  *" ++ parseArray xs
	| x == 3 = "\n   *" ++ parseArray xs
	| x == 4 = "\n    *" ++ parseArray xs
	| x == 5 = "\n     *" ++ parseArray xs
	| x == 6 = "\n      *" ++ parseArray xs
	| x == 7 = "\n       *" ++ parseArray xs
	| x == 8 = "\n        *" ++ parseArray xs
	| x == 9 = "\n         *" ++ parseArray xs
	| x >= 10 = [] ++ parseArray xs 
-}
	
-}