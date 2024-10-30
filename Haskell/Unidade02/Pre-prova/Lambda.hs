{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Lambda where

--Funções Lambda:
--Ideal quando a função é usada apenas uma vez

--Exemplo 1:
--Função que retorna o dobro de um número
dobro :: Int -> Int
dobro = \x -> 2*x

--Exemplo 2:
--Função que retorna o quadrado de um número
quadrado :: Int -> Int
quadrado = \x -> x*x

--Exemplo 3:
--Função que aplica multiplos lambdas
aplica :: (Int -> Int) -> Int -> Int
aplica = \f x -> f x


