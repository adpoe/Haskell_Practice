applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right) 
   | abs ((left + n) - right) < 4 = Just (left + n,right)
   | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right) 
   | abs (left - (right + n)) < 4 = Just (left,right + n)
   | otherwise                    = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

x -: f = f x 

foo :: Maybe String
foo = Just 3   >>= (\x ->
	  Just "!" >>= (\y ->
	  Just (show x ++ y)))

foo' :: Maybe String
foo' = do 
	x <- Just 3
	y <- Just "!"
	Just (show x ++ y)

marySue :: Maybe Bool
marySue = do
	x <- Just 9
	Just (x > 8)

routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second	

justH :: Maybe Char
justH = do
   (x:xs) <- Just "hello"
   return x    

listOfTuples :: [(Int,Char)]
listOfTuples = do
   n <- [1,2]
   ch <- ['a','b']
   return (n,ch)

type KnightPos = (Int,Int) 

moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])  
    return (c',r')  