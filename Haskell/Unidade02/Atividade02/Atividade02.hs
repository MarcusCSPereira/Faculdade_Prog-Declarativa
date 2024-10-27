{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use odd" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}
module Atividade02 where
import Trace.Hpc.Mix (BoxLabel)
import Text.XHtml (base)


--Q1_a)
quadrados :: [Int]
quadrados = [x^2 | x <- [1..20]]

--Q1_b)
impares :: [Int]
impares = [x | x <- [1..50], mod x 2 /= 0]

--Q1_c)
multiplos3e5 :: [Int]
multiplos3e5 = [x | x <- [1..100], teste x]
  where
    teste x = mod x 3 == 0 && mod x 5 == 0

--Q1_d)
divisiveis7 :: [Int]
divisiveis7 = [x | x <- [1..100], divisao x]
  where
    divisao x = mod x 7 == 0

--Q1_e)
palindromos :: [Int]
palindromos = [x | x <- [1..1000], palindromo x]
  where
    palindromo x = show x == reverse (show x)

--Q2_a)

mapeamento :: (a -> b) -> [a] -> [b]
mapeamento f [] = []
mapeamento f (x:xs) = f x : mapeamento f xs

filtro :: (a -> Bool) -> [a] -> [a]
filtro f [] = []
filtro f (a:b)
  | f a = a : filtro f b
  | otherwise = filtro f b

reduceR :: (a->a->a) -> [a] -> a
reduceR f [a] = a
reduceR f (a:b) = f a (reduceR f b)

reduceL :: (a->a->a) -> [a] -> a
reduceL f [a] = a
reduceL f (a:b) = reduceL f (f a (head b) : tail b)

dobrar :: [Int] -> [Int]
dobrar lst = mapeamento (*2) lst

--Q2_b)

filtraPares :: [Int]
filtraPares = filtro (even) [1..20]

--Q2_c)

somaElementos :: [Int] -> Int
somaElementos lst = reduceL (+) lst

--Q2_d)

filtraEdobra :: [Int] -> [Int]
filtraEdobra lst = map (*2) (filter (even) lst)

--Q2_e)

produtoDeTodos :: [Int] -> Int
produtoDeTodos lst = reduceL (*) lst

--Q2_f)
--Note que o primeiro que chamamos representa a ultima operação que será feita 
soma1 :: [Int] -> [Int]
soma1 lst = (map (+1) . filter (>5)) lst

--Q3

myAll :: [a] -> (a -> Bool) -> Bool
myAll [] f = True
myAll (a:b) f
  | f a = myAll b f
  | otherwise = False

--Q4_a)
inverte :: [a] -> [a]
inverte [] = []
inverte (x:b) = inverte b ++ [x]

--Q4_b)
removeUltimo :: [a] -> [a]
removeUltimo [] = []
removeUltimo [elem] = []
removeUltimo (x:y) = x : removeUltimo y

--Q4_c)
segundoElem :: [a] -> [a]
segundoElem [] = []
segundoElem [elem] = []
segundoElem (x:y:z) = [y]

--Q_6:
concat':: [[a]] -> [a]
concat' [] = []
concat' (a:b) = a ++ concat' b

--Q_7:

mapish :: [(a->b)] -> a -> [b]
mapish [] x = []
mapish (a:b) x = mapeamento (a) [x] ++ mapish b x



