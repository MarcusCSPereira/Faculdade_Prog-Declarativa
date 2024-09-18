{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Use odd" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use :" #-}
module Prova1 where
import System.Posix (accessModes)
import Data.Time.Format.ISO8601 (yearFormat)
import Distribution.Simple.Utils (xargs)


--Maior e menor:

maiorEMenor :: [Int] -> (Int, Int)
maiorEMenor [elem] = (elem,elem)
maiorEMenor list = (maior list, menor list)

maior :: [Int] -> Int
maior [elem] = elem 
maior (a:b) = maiorAux a b

maiorAux :: Int -> [Int] -> Int
maiorAux m [] = m
maiorAux m (a:b)
  | a > m = maiorAux a b
  | otherwise = maiorAux m b

menor :: [Int] -> Int
menor [elem] = elem
menor (a:b) = menorAux a b

menorAux :: Int -> [Int] -> Int
menorAux m [] = m
menorAux m (a:b)
  | a < m = menorAux a b
  | otherwise = menorAux m b

---------------------------------------------

areaTri :: Float -> Float -> Float
areaTri base altura = (base * altura) / 2

menorEntre3 :: Int -> Int -> Int -> Int
menorEntre3 a b c
  | a > b && b > c = c
  | b > a && c > a = a
  | otherwise = b

diferentes :: Int -> Int -> Int -> Bool
diferentes a b c
  | a /= b || b /= c || c/= a = True
  | otherwise = False

somaAlgorismos :: Int -> Int
somaAlgorismos n = resultado
  where
    resultado = (div n 100) + (div (mod n 100) 10) + (mod n 10)

--Lista 02:
somaPares :: Int -> Int
somaPares 0 = error "Valor Inválido"
somaPares 1 = 0
somaPares n
  | n < 0 = error "Valor Inválido"
  | mod n 2 == 0 = n + somaPares (n-1)
  | mod n 2 /= 0 = somaPares (n-1)
  
-- 4 -> 4 + somaPares(3) = 4 + 2 = 6
-- 3 -> somaPares(2) -> 2
-- 2 -> 2 + somaPares(1) -> 2 + 0 = 2
-- 1 -> 0

elevado :: Int -> Int -> Int
elevado _ 0 = 1
elevado 0 _ = 0
elevado x 1 = x
elevado x y = x * (elevado x (y-1))

somaFatoriais :: Int -> Int
somaFatoriais 1 = 1
somaFatoriais n = fatorial n + somaFatoriais (n-1)
  where
    fatorial x = fatorialAux 1 x


fatorialAux :: Int -> Int -> Int
fatorialAux acc 0 = acc
fatorialAux acc x = fatorialAux (acc*x) (x-1)

somaAlgorismos' :: Int -> Int
somaAlgorismos' n
  | div n 10 == 0 = n
  | otherwise = mod n 10 + somaAlgorismos' (div n 10)

fibonaci :: Int -> Int
fibonaci n = fiboAux n 0 1

fiboAux :: Int -> Int -> Int -> Int
fiboAux 0 acc suc = acc
fiboAux n acc suc = fiboAux (n-1) suc (acc+suc)

-- Lista e Tupla:

type Aluno = (Matricula,Nome,Idade)

type Alunos = (Aluno)

type Matricula = Int
type Nome = String
type Idade = Int

type Ponto = (Float,Float)

calcularDistancia :: Ponto -> Ponto -> Float
calcularDistancia (x,y) (a,b) = sqrt ((a-x)^2 + (b-y)^2)

minMax :: Int -> Int -> Int -> (Int,Int)
minMax x y z
  | x > y && y > z = (z,x)

tamanhoLista :: [Int] -> Int
tamanhoLista [] = 0
tamanhoLista (h:t) = 1 + tamanhoLista t

--1,2,3,4 = h -> 1 Int | t -> 2,3,4 [Int]

--list | (h:t)

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista [elem] = elem
somaLista (h:t) = h + somaLista t

dobraLista :: [Int] -> [Int]
dobraLista [] = []
dobraLista [elem] = [elem * 2]
dobraLista (h:t) = (h*2) : dobraLista t

--elem -> lista = elem : lista -> Lista
--lista juntar lista = lista ++ lista -> Lista 
--lista -> lista = lista : lista -> Lista dentro de uma Lista

estaDentro :: Int -> [Int] -> Bool
estaDentro x [] = False
estaDentro x (h:t)
  | x == h = True
  | otherwise = estaDentro x t

numVogais :: String -> Int
numVogais [] = 0
numVogais (h:t)
  | h == 'a' || h == 'e' || h == 'i' = 1 + numVogais t 
  | otherwise = numVogais t

palavras :: String -> Int
palavras [] = 0
palavras str = contaPalavra (split str)
  where
    contaPalavra [] = 0
    contaPalavra (a:b) = 1 + contaPalavra b

split :: String -> [String]
split [] = []
split str = pegaPalavra str : split (discartaPalavra str)

pegaPalavra :: String -> String
pegaPalavra [] = []
pegaPalavra (a:b)
  | a == ' ' = []
  | otherwise = a : pegaPalavra b

discartaPalavra :: String -> String
discartaPalavra [] = []
discartaPalavra (a:b)
  | a == ' ' = b
  | otherwise = discartaPalavra b

reverteString :: String -> String
reverteString [] = []
reverteString [elem] = [elem]
reverteString (h:t) = reverteString t ++ [h]

maioL :: [Int] -> Int
maioL [] = 0
maioL [elem] = elem
maioL (h:t) = maiorAux h t


minEmax :: [Int] -> (Int,Int)
minEmax list = (menoL list, maioL list)

menoL :: [Int] -> Int
menoL [] = 0
menoL (h:t) = menoAux h t

menoAux :: Int -> [Int] -> Int
menoAux menor [] = menor
menoAux menor (h:t)
  | h < menor = menoAux h t
  | otherwise = menorAux menor t

-----------------------------------------------

type Preco = Int
type CodigoBarra = Int
type BancoDeDados = [(CodigoBarra, Nome, Preco)]

bd :: BancoDeDados
bd = [ (1001,"Refrigerante",450),
       (1002,"Leite",320),
       (1003,"Biscoito",200),
       (1004,"Suco",989),
       (1005,"Arroz",345),
       (1006,"Feijao",780)
     ]

buscarBDaux :: CodigoBarra -> BancoDeDados -> (Nome,Preco)
buscarBDaux x [] = error "Valor não encontrado"
buscarBDaux x ((cb,n,p):t)
  | x == cb = (n,p)
  | otherwise = buscarBDaux x ((cb,n,p):t)

------------------------------------------------
--Prova Antiga:

--Questão 01:
calcularDias :: [String] -> Int
calcularDias [a,b] = diasTotal (converter b) - diasTotal (converter a)

diasTotal :: (Int,Int,Int) -> Int
diasTotal (d,m,a) = d + (m*31) + (a*365) 

converter :: String -> (Int,Int,Int)
converter str = extrair (splitp str)

pPalavra :: String -> String
pPalavra [] = []
pPalavra (a:b)
  | a /= '/' = a : pPalavra b
  | otherwise = []

dPalavra :: String -> String
dPalavra [] = []
dPalavra (a:b)
  | a == '/' = b
  | otherwise = dPalavra b

splitp :: String -> [String]
splitp [] = []
splitp list = pPalavra list : splitp (dPalavra list)

extrair :: [String] -> (Int,Int,Int)
extrair [a,b,c] = (read a, read b, read c)

type NumeroCaracteres = Int
type NumeroPalavras = Int
type NumeroLinhas = Int
type NumeroVogais = Int
type NumEspacos = Int
type QuantNumeros = Int

contar :: String -> (NumeroCaracteres,NumeroPalavras,NumeroLinhas,NumeroVogais,NumEspacos,QuantNumeros)
contar [] = (0,0,0,0,0,0)
contar str = (numChar str, numP str, numL str, numVog str, numE str, qtdNum str)

numChar :: String -> Int
numChar [] = 0
numChar (a:b) = 1 + numChar b

numP :: String -> Int
numP [] = 0
numP str = contapalavra (splt str)
  where
    contapalavra [] = 0
    contapalavra (a:b) = 1 + contapalavra b
    splt [] = []
    splt s = pegaPal s : splt (removePal s)
      where
        pegaPal [] = []
        pegaPal (a:b)
          | a /= ' ' = a : pegaPal b
          | otherwise = []
        removePal [] = []
        removePal (a:b)
          | a == ' ' = b
          |otherwise = removePal b

numL :: String -> Int
numL [] = 0
numL str = contaLinha (splitL str)
  where
    contaLinha [] = 0
    contaLinha (a:b) = 1 + contaLinha b
    splitL [] = []
    splitL s = pegaLinha s : splitL (removeLinha s)
      where
        pegaLinha [] = []
        pegaLinha (a:b)
          | a /= '\n' = a : pegaLinha b
          | otherwise = []
        removeLinha [] = []
        removeLinha (a:b)
          | a == '\n' = b
          | otherwise = removeLinha b

numVog :: String -> Int
numVog [] = 0
numVog (a:b)
  | a == 'a' || a == 'e' || a == 'i' || a == 'o' || a == 'u' = 1 + numVog b
  | otherwise = numVog b

numE :: String -> Int
numE [] = 0
numE (a:b)
  | a == ' ' = 1 + numE b
  | otherwise = numE b

qtdNum :: String -> Int
qtdNum [] = 0
qtdNum (a:b)
  | a == '1' || a == '2' || a == '3' || a == '4' || a == '5' || a == '6' || a == '7' || a == '8' || a == '9' || a == '0' = 1 + qtdNum b
  | otherwise = qtdNum b

-- ord tranforma um char em um numero, e chr tranforma um numero em um char, show tranforma um numero em uma string e read tranforma uma string em um numero, isDigit verifica se é um digito, isAlpha verifica se é uma letra, isSpace verifica se é um espaço, length verifica o tamanho de uma lista, head retorna a cabeça de uma list, tail retorna o corpo de uma lista, last retorna o ultimo elemento de uma lista, init retorna a lista sem o ultimo elemento, take pega os n primeiros elementos de uma lista, drop remove os n primeiros elementos de uma lista, splitAt divide uma lista em duas, concat concatena listas, reverse reverte uma lista, and verifica se todos os elementos de uma lista são verdadeiros, or verifica se algum elemento de uma lista é verdadeiro, all verifica se todos os elementos de uma lista satisfazem uma condição, any verifica se algum elemento de uma lista satisfaz uma condição

intercala :: [Int] -> [Int] -> [Int]
intercala [] y = y
intercala x [] = x
intercala (x:xs) (y:ys)
  | x < y = x : intercala xs (y:ys) 
  | otherwise = y : intercala (x:xs) ys

--Nome:

last' :: [a] -> a
last' [x] = x
last' (_:t) = last' t

head' :: [a] -> a
head' (h:_) = h

init' :: [a] -> [a]
init' [_] = []
init' (h:t) = h : init' t

abreviarNomesIntermediarios :: [String] -> [String]
abreviarNomesIntermediarios [] = []
abreviarNomesIntermediarios (x:xs) = [head' x : "."] ++ abreviarNomesIntermediarios xs

abreviar :: String -> String
abreviar nomeCompleto =
  let partes = split nomeCompleto          -- Divide o nome em partes
      primeiroNome = head' partes          -- Pega o primeiro nome
      ultimoNome = last' partes            -- Pega o último nome
      nomesIntermediarios = abreviarNomesIntermediarios (init' (tail partes))  -- Abrevia os nomes do meio
  in unwords' (primeiroNome : nomesIntermediarios ++ [ultimoNome])

-- Função para juntar uma lista de strings em uma única string separada por espaços
unwords' :: [String] -> String
unwords' [] = ""
unwords' [x] = x
unwords' (x:xs) = x ++ " " ++ unwords' xs




