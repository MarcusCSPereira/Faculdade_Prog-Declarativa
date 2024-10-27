{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Eta reduce" #-}
module Atividade01 where

--Funções de Alta Ordem e Compreensão de Listas:

--Função de mapeamento, ex de chamada: mapeamento (*2) [1,2,3,4,5]
mapeamento :: (a -> b) -> [a] -> [b]
mapeamento func lista = [func elem | elem <- lista]

--Função de  filtragem, ex de chamada: filter (>0) [1,2,3,-3,-5,-6,8]
filter :: (a -> Bool) -> [a] -> [a]
filter func lista = [elem | elem <- lista, func elem]

--Função de redução, ex de chamada: reducao (+) 0 [1,2,3,4,5]
reduzir :: (Int -> Int -> Int) -> Int -> [Int] -> Int
reduzir f v [] = v
reduzir f v (a:b) = f a ( reduzir f v b)

takewhile :: (a -> Bool) -> [a] -> [a]
takewhile f [] = []
takewhile f (a:b)
  | f a = a : takewhile f b
  | otherwise = []

--Exemplo do takeWhile:
-- takeWhile (<5) [1,2,3,4,5,6,7,8,9]
-- [1,2,3,4]

--Exemplo de composição de funções: (Note que a chamada de composição é feito da seguinte forma: a primeira é a ultima função a ser chamada, e a ultima é a primeira, então construimos de trás pra frente)

-- (foldl (+) 0 . map func) "haskell"
-- foldl (+) 0 (map func "haskell")
-- foldl (+) 0 (map func ['h','a','s','k','e','l','l'])
-- fold (+) 0 [1,1,1,1,1,1,1]
-- 7

--Classes: (Note que usaremos classes quando necessitamos de Polimorfismo)

--Para ver informações sobre uma classe faça no ghci o seguinte comando: info NomeDaClasse | EX: info Eq | info Ord | info Num | info Show | info Enum

todosIguais :: Eq a => [a] -> Bool
todosIguais [] = True
todosIguais [a] = True
todosIguais (a:b) = and [a == x | x <- b]

--Eq para valores de tipos que podem ser comparados por igualdade
todosIguais' :: Eq a => a -> a -> a -> Bool
todosIguais' a b c = a == b && b == c

--Ord para valores de tipos que podem ser comparados por ordem
maior3 :: Ord a => a -> a -> a -> a
maior3 a b c = max a (max b c)

--Enum para valores de tipos que podem ser enumerados
sucessor :: Enum a => a -> a
sucessor a = succ a

--Show para valores de tipos que podem ser convertidos em strings
mostrar :: Show a => a -> String
mostrar a = show a

--Num para valores de tipos numéricos 
somar :: Num a => a -> a -> a
somar x y = x + y 

