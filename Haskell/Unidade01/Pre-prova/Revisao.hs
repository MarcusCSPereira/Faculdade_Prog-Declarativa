{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use foldr" #-}
module Revisao where

type Nome = String
type Preco = Int
type CodigoBarra = Int
type BancoDeDados = [(CodigoBarra, Nome, Preco)]

bd :: BancoDeDados
bd = [(1001,"Refri",450),
      (1002,"Leite",320),
      (1003,"Biscoito",200),
      (1004, "Suco",989),
      (1005,"Arroz",345),
      (1006,"Feijão",780)
      ]
--Q1:

buscarBDaux :: CodigoBarra -> BancoDeDados -> (Nome, Preco)
buscarBDaux x [] = error "Valor não encontrado"
buscarBDaux x ((cb,n,p):t)
  | cb == x = (n, p)
  | otherwise = buscarBDaux x t

codigo :: (CodigoBarra,Nome,Preco) -> CodigoBarra
codigo (c,n,p) = c
nome :: (CodigoBarra,Nome,Preco) -> Nome
nome (c,n,p) = n
preco :: (CodigoBarra,Nome,Preco) -> Preco
preco (c,n,p) = p

codigos :: BancoDeDados -> [CodigoBarra]
codigos [] = []
codigos ((cb,n,p):b) = cb : codigos b

--Q2:

buscarBd :: CodigoBarra -> (Nome,Preco)
buscarBd c = buscarBDaux c bd 

--Q3:

fazerConta :: [CodigoBarra] -> [(Nome, Preco)]
fazerConta [] = []
fazerConta (a:b) = buscarBd a : fazerConta b

--Q4:
dividir :: Int -> String
dividir 0 = "0"
dividir x = r ++ "." ++ dc ++ c
  where
    r = show (div x 100)
    dc = show (mod (div x 10) 10)
    c = show (mod x 10)

--Mod retorna os valores da direita e div da esquerda, e a gente usa o div pra diminuir o número, o mod retorna um valor, o div pode retornar mais de um valor

--Q5:
repetir :: Int -> String -> String
repetir 0 c = ""
repetir n c = repetir (n-1) c ++ "."

--Q6:
tamanhoLinha :: Int
tamanhoLinha = 30

formatarLinha :: (Nome, Preco) -> String
formatarLinha (n,p) = n ++ repetir tamanhoCorreto "." ++ cifrao ++ dividir p ++ "\n" 
  where 
    cifrao = " R$"
    tamanhoCorreto = tamanhoLinha - length n - length (dividir p) - length cifrao

--Q7:
formatarLinhas :: [(Nome,Preco)] -> String
formatarLinhas [] = ""
formatarLinhas (a:b) = formatarLinha a ++ formatarLinhas b

--Q8:
calcularTotal :: [(Nome, Preco)] -> Int
calcularTotal [] = 0
calcularTotal ((n,p):b) = p + calcularTotal b

--Q9:
formatarTotal :: Int -> String
formatarTotal p = formatarLinha (n,p)
  where
    n = "Total:"

--Q10:
formatarConta :: [(Nome,Preco)] -> String
formatarConta list = formatarLinhas list ++ formatarTotal (calcularTotal list)

imprimirConta :: [CodigoBarra] -> IO()
imprimirConta lista = putStr (formatarConta (fazerConta lista))

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted (a:b)
  | menor a b == a = isSorted b
  | otherwise = False

menor :: Int -> [Int] -> Int
menor m [] = m
menor m (a:b)
  | a < m = menor a b
  | otherwise = menor m b

type Resultado = [Int]
type Jogos = [[Int]]

premiados :: Resultado -> Jogos -> Int
premiados _ [] = 0
premiados r (x:y)
  | premiado r x = 1 + premiados r y
  | otherwise = premiados r y

premiado :: Resultado -> [Int] -> Bool
premiado [] [] = True
premiado (x:y) (a:b)
  | x == a = premiado y b
  | otherwise = False 
premiado _ _ = False

quantidadeVendidaEstado :: String -> [(String,String, Int)] -> Int
quantidadeVendidaEstado _ [] = 0
quantidadeVendidaEstado es ((e,c,v):b)
  | es == e = v + quantidadeVendidaEstado es b
  | otherwise = quantidadeVendidaEstado es b

quantidadeTotalVendida :: [(String,String, Int)] -> Int
quantidadeTotalVendida [] = 0
quantidadeTotalVendida ((e,c,v):b) = v + quantidadeTotalVendida b

cidadesNoEstado :: String -> [(String,String, Int)] -> [String]
cidadesNoEstado _ [] = []
cidadesNoEstado es ((e,c,v):b)
  | es == e = c : cidadesNoEstado es b
  | otherwise = cidadesNoEstado es b