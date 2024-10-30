{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Generalizacao where

--Compreensão de listas:
--Ideal quando o retorno é uma lista
dobro :: [Int] -> [Int]
dobro xs = [2*x | x <- xs]

dobroPares :: [Int] -> [Int]
dobroPares xs = [2*x | x <- xs, ehPar x]

ehPar :: Int -> Bool
ehPar x = mod x 2 == 0

--Funções de alta ordem:

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f xs = [f x | x <- xs]

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (a:b)
  | f a = a : filter' f b
  | otherwise = filter' f b

reduceL :: (a -> a -> a) -> [a] -> a
reduceL f [a] = a
reduceL f (a:b) = reduceL f (f a (head b) : tail b)

reduceR :: (a -> a -> a) -> [a] -> a
reduceR f [a] = a
reduceR f (a:b) = f a (reduceR f b)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (a:b)
  | f a = a : takeWhile' f b
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (a:b)
  | f a = dropWhile' f b
  | otherwise = a:b

concat' :: [[a]] -> [a]
concat' [] = []
concat' (a:b) = a ++ concat' b

mapish :: ([a -> b]) -> a -> [b]
mapish [] _ = []
mapish (a:b) x = map' (a) [x] ++ mapish b x