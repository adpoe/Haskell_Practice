data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
    deriving Show
    

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]       

isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False

isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False 
isSmall2 _    = True

data FailableDouble = Failure
                    | OK Double 
     deriving Show 

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

faulureToZero :: FailableDouble -> Double 
faulureToZero Failure = 0
faulureToZero (OK d)  = d 


-- Store a person's name, age, and favorite Thing
data Person = Person String Int Thing
  deriving Show

richard :: Person 
richard = Person "Richard" 32 Ship

stan :: Person 
stan    = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a 


data AlgDataType = Constr1 Type11 Type12
                 | Constr2 Type21 
                 | Constr3 Type31 Type32 Type33
                 | Constr4

baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n   


checkFav :: Person -> String 
checkFav (Person n _ Ship) = n ++ ", you're my favorite kind of person!"
checkFav (Person n _ _)    = n ++ ", your favorite thing is lame."

let ex02 = case "Hello" of
	[]         -> 3
	('H':s)    -> length s 
	_          -> 7

failureToZero' :: FailableDouble -> Double
faulureToZero' x = case x of
                     Failure -> 0
                     OK d -> d 

data LogMessage = LogMessage Int String
data MaybeLogMessage = ValidLM LogMessage
                     | InvalidLM
data MaybeInt = ValidInt Int
              | InvalidInt 

data Maybe a = Just a
             | Nothing

example_a = Maybe Int -> Int
example a (Just n)  = n
example_a Nothing   = -1

example_b :: LogMessage -> Maybe String 
example_b (LogMessage severity s) | severity >= 50 Just s
example_b _ 

data List t = Empty | Cons t (List t)

lst1 :: List Int
lst1 = Cons 3 (Cons 5 (Cons 2 empty)) 

lst2 :: List Char
lst2 = Cons 'x' (Cons 'y' (Cons 'z' Empty))

lst3 :: List Bool
lst3 = Cons True (Cons False Empty)

intListProd :: List Int -> Int
intListProd Empty        = 1
intListProd (Cons x 1)   = x * intListProd 1

data Tree = Leaf Char
          | Node Tree Int Tree
    deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (leaf 'y')) 2 (Leaf 'z')                                           

 









