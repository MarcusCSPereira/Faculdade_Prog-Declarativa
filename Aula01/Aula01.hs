module Aula01 where
import Data.Char

quadrado :: Int -> Int
quadrado x = x*x

x :: Integer
x= 5 

maior :: Int -> Int -> Int
maior  x y
    | x >= y = x
    | x < y = y

maior' :: Int -> Int -> Int
maior'  x y
    | x >= y = x  -- o nome disso são guarders/guardas
    | otherwise = y

maior'' :: Int -> Int -> Int
maior'' x y =
    if x>=y then x else y

cubo :: Int -> Int
cubo x = quadrado x * x

todosIguais :: Int -> Int -> Int -> Bool
todosIguais x y z = (x == y) && (y == z)

testeTodosIguais :: Bool
testeTodosIguais =
    (todosIguais 2 2 2 == True) &&
    (todosIguais 2 3 2 == False) &&
    (todosIguais 3 2 2 == False)

main :: IO ()
main = print (maior 3 5)