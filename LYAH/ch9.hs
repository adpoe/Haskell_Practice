import Data.Char
import Data.List

myAction :: IO String
myAction = do
	a <- getLine
	b <- getLine
	return $ a ++ b

myAction' :: IO String
myAction' = (++) <$> getLine <*> getLine

instance Monoid [a] where
	mempty = []
	mappend = (++)