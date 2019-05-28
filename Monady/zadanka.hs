-- import Control.Monad.Writer
import Data.Maybe
-- import Prelude hiding (lookup)
-- Zadanie 80


-- Zadanie 81 (Wilkosz)
-- myGCD :: Int -> Int -> Writer [String] Int
-- myGCD a 0 = writer (a, ["result = " ++ show a])
-- myGCD a b = do
--   tell [show a ++ " mod " ++ show b ++ " = " ++ show modulo]
--   prev <- myGCD b modulo
--   return prev
--   where modulo = a `mod` b

-- Zadanie 67 (Muminek) // maybe :: Maybe b -> (a -> b)
(>>=) :: Maybe Float -> (Float -> Maybe Float) -> Maybe Float
mx >>= f = maybe Nothing f mx

(>=>) :: ( Float -> Maybe Float) -> (Float -> Maybe Float) -> (Float -> Maybe Float)
f >=> g = (\x -> (f x) Main.>>= g)

join ttx = ttx Main.>>= (fmap id) Just

-- zdefiniujmy 