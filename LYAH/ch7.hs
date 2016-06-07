import qualified Data.Map as Map 
data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)  

data Car a b c = Car { company :: a
                     , model :: b
                     , year :: c
                     } deriving (Show)

surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape  
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b)) 

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r 

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

data Person = Person { firstName :: String
                     , lastName :: String
	                 , age :: Int
                     } deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)] 

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name  pnumber pbook = (name,pnumber) `elem` pbook

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
	case Map.lookup lockerNumber map of
		Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
		Just (state, code) -> if state /= Taken
			                    then Right code
			                    else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]  

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)


infixr 5 .++
(.++) :: List a -> List a -> List a 
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x 
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)


treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left 
    | x > a  = treeElem x right 

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
	Red == Red = True
	Green == Green = True
	Yellow == Yellow = True
	_ == _ = False


instance Show TrafficLight where
	show Red = "Red light"
	show Yellow =  "Yellow light"
	show Green = "Green light"


class YesNo a where
    yesno :: a -> Bool	

instance YesNo Int where
	yesno 0 = False
	yesno _ = True 

instance YesNo [a] where
	yesno [] = False
	yesno _  = True 

instance YesNo Bool where
	yesno = id

instance YesNo (Maybe a) where
	yesno (Just _) = True
	yesno Nothing  = False 

instance YesNo (Tree a) where
	yesno EmptyTree = False
	yesno _ = True 

instance YesNo TrafficLight where
	yesno Red = False
	yesno _ = True 

yesnoIf :: (YesNo y) => y -> a -> a -> a 
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

class Func f where
	funcMap :: (a -> b) -> f a -> f b 

instance Functor Tree where
	fmap f EmptyTree = EmptyTree
	fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

class Tofu t where
	tofu :: j a -> t a j 

data Frank a b = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
    tofu x  = Frank x








