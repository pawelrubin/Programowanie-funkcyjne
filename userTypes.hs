-- typy wyliczeniowe
data Color = Green | Yellow | Red deriving (Show, Eq)

nextC :: Color -> Color
nextC Green = Yellow
nextC Yellow = Red
nextC Red = Green

-- record-like types
data Pracownik1 = Pracownik1 String String Int deriving Show

data Pracownik = Pracownik {
  pracImie :: String,
  pracNazwisko :: String,
  pracPensja :: Int
} deriving Show

-- typy rekurencyjne i parametryczne
data Tree a = L a | N (Tree a) a (Tree a)

-- robimy funktor
instance Functor Tree where
  fmap f (L x) = L (f x)
  fmap f (N left x right) = N (fmap f left) (f x) (fmap f right)

-- wlasne show
instance (Show a) => Show (Tree a) where
  show (L x) = "<" ++ show x ++ ">"
  show (N l x r) = "[" ++ show l ++ ", " ++ show x ++ ", " ++ show r ++ "]"

-- robimy foldable
instance Foldable Tree where
  foldr f z (L x) = f x z
  foldr f z (N left x right) = foldr f (f x (foldr f z right)) left


class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

-- Funktor Maybe
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

