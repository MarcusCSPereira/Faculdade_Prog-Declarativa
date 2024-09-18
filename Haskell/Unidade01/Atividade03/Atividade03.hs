{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use max" #-}
{-# HLINT ignore "Use min" #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Redundant if" #-}
module Atividade03 where

import Data.Char

--Questão 01
type Ponto = (Float, Float)

distancia :: Ponto -> Ponto -> Float
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

--Questão 02
type Centro = Ponto
type Raio = Float
type Circulo = (Centro, Raio)

estaDentro :: Ponto -> Circulo -> Bool
estaDentro (x, y) ((cx, cy), r) = distancia (x, y) (cx, cy) <= r

--Questão 03
minemax :: (Int,Int,Int) -> (Int,Int)
minemax (x,y,z)
  | x > y && y > z = (z,x)
  | x > z && z > y = (y,x)
  | y > x && z > x = (x,y)
  | y > z && x > z = (z,y)
  | z > x && x > y = (y,z)
  | z > y && y > x = (x,z)


--Questão 04
maxocorre :: (Int,Int,Int) -> (Int,Int)
maxocorre (x,y,z)
  | x > y && x > z = (x, if (y > z) || (z > y) then 1 else 2)

--Questão 05
tamLista :: [Int] -> Int
tamLista [] = 0
tamLista (a:b) = 1 + tamLista b

--Questão 06
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (a:b) = a + somaLista b

--Questão 07
duplica :: [Int] -> [Int]
duplica [] = []
duplica (a:b) = (a*2):duplica b

--Questão 08
pertence :: [Int] -> Int -> Bool
pertence [] _ = False
pertence (a:b) x = if a == x then True else pertence b x

--Questão Exemplo:
conta :: [Int] -> Int -> Int
conta [] _ = 0
conta (a:b) x = if a == x then 1 + conta b x else conta b x

--Questão 09: (obs)
insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (a:b) = ins a (insertionSort b)

ins :: Int -> [Int] -> [Int]
ins a [] = [a]
ins a (b:c)
  | a <= b = a:b:c
  | otherwise = b:ins a c

--Questão 10: (obs)
selectionSort :: [Int] -> [Int]
selectionSort [] = []
selectionSort a = m:selectionSort (remove m a)
  where m = minimo a

minimo :: [Int] -> Int
minimo [a] = a
minimo (a:b)
  | a <= m = a
  | otherwise = m
  where m = minimo b

remove :: Int -> [Int] -> [Int]
remove _ [] = []
remove a (b:c)
  | a == b = c
  | otherwise = b:remove a c

--Questão 11: (obs)
mergeSort :: [Int] -> [Int] -> [Int]
mergeSort [] [] = []
mergeSort [] b = b
mergeSort a [] = a
mergeSort (a:b) (c:d)
  | a < c = a:mergeSort b (c:d)
  | otherwise = c:mergeSort (a:b) d

--Questão 12: (obs)
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (a:b) = quickSort [x | x <- b, x <= a] ++ [a] ++ quickSort [x | x <- b, x > a]

--Questão 13:
numVogais :: String -> Int
numVogais [] = 0
numVogais (a:b) = if a == 'a' || a == 'e' || a == 'i' || a == 'o' || a == 'u' then 1 + numVogais b else numVogais b

--Questão 14:
contaPal :: String -> Int
contaPal [] = 0
contaPal (a:b) = if a == ' ' then 1 + contaPal b else contaPal b

--Questão 15:
cifraCesar :: Int -> String -> String
cifraCesar _ [] = []
cifraCesar x (a:b) = converteAlfabeto (ord a + x) : cifraCesar x b

converteAlfabeto :: Int -> Char
converteAlfabeto y
  | y >= ord 'a' && y <= ord 'z' = chr (ord 'a' + mod (y - ord 'a') 26)
  | y >= ord 'A' && y <= ord 'Z' = chr (ord 'A' + mod (y - ord 'A') 26)

--Questão 16:
inverterString :: String -> String
inverterString [] = []
inverterString (a:b) = inverterString b ++ [a]

--Questão 17:
converteFrase :: String -> String
converteFrase [] = []
converteFrase (a:b) = Data.Char.toUpper a : converteResto b
  where
    converteResto [] = []
    converteResto (x:y)
      | x == ' ' = ' ' : converteFrase y
      | otherwise = Data.Char.toLower x : converteResto y

--Questão 18:
buscaBinaria :: [Int] -> Int -> Int -> Int -> Int
buscaBinaria lista alvo inicio fim
  | inicio > fim = -1
  | lista !! meio == alvo = meio
  | lista !! meio < alvo = buscaBinaria lista alvo (meio + 1) fim
  | otherwise = buscaBinaria lista alvo inicio (meio - 1)
    where
      meio = (inicio + fim) `div` 2

buscar :: [Int] -> Int -> Int
buscar lista alvo = buscaBinaria lista alvo 0 (length lista - 1)

--Questão 19:
maiorElemento :: [Int] -> Int
maiorElemento [x] = x
maiorElemento (a:b) =
  let maiorRestante = maiorElemento b
  in if a > maiorRestante then a else maiorRestante

--Questão 20:
maiorEmenor :: [Int] -> (Int,Int)
maiorEmenor [x] = (x,x)
maiorEmenor (a:b) =
  let (maiorRestante,menorRestante) = maiorEmenor b
  in (if a > maiorRestante then a else maiorRestante, if a < menorRestante then a else menorRestante)

--Questão 21:
numLeP :: String -> (Int,Int)
numLeP [] = (0,0)
numLeP str = (contaPalavra str,contaLetra str)

contaPalavra :: String -> Int
contaPalavra [] = 0
contaPalavra (' ':b) = contaPalavra b
contaPalavra (_:b) = 1 + contaPalavra (pularPalavra b)

pularPalavra :: String -> String
pularPalavra [] = []
pularPalavra (' ':b) = b
pularPalavra (_:b) = pularPalavra b

contaLetra :: String -> Int
contaLetra [] = 0
contaLetra (a:b)
  | a /= ' ' = contaLetra b + 1
  | otherwise = contaLetra b

--Questão 22:
palindromo :: String -> Bool
palindromo str
  | str == reverteString str = True
  | otherwise = False

reverteString :: String -> String
reverteString [] = []
reverteString (a:b) = reverteString b ++ [a]

--Questão 23:
abreviar :: String -> String
abreviar nome = abreviarAux (split nome) 

abreviarAux :: [String] -> String
abreviarAux [primeiro] = primeiro
abreviarAux [primeiro,ultimo] = primeiro ++ " " ++ ultimo
abreviarAux (primeiro:meio:resto) = primeiro ++ " " ++ abreviarIntermediarios (meio:resto)

abreviarIntermediarios :: [String] ->String
abreviarIntermediarios [ultimo] = ultimo
abreviarIntermediarios (a:b)
  | a == "Dos" || a == "De" = abreviarIntermediarios b
  | otherwise = [a !! 0] ++ ". " ++ abreviarIntermediarios b

--Questão 24:
numeroPerfeito :: Int -> Bool
numeroPerfeito n = somaDivisores n (n-1) == n

somaDivisores :: Int -> Int -> Int
somaDivisores n 0 = 0
somaDivisores n x
  | n `mod` x == 0 = x + somaDivisores n (x-1)
  | otherwise = somaDivisores n (x-1)

--Questão 25:
intercala :: [Int] -> [Int] -> [Int]
intercala [] ys = ys
intercala xs [] = xs
intercala (x:xs) (y:ys)
  | x < y = x : intercala xs (y:ys)
  | otherwise = y : intercala (x:xs) ys

--Questão 26:
maisDe10 :: [Int] -> Bool
maisDe10 [] = False
maisDe10 list
  |contaElementos list > 10 = True
  |otherwise = False

contaElementos :: [Int] -> Int
contaElementos [] = 0
contaElementos (a:b) = 1 + contaElementos b

--Questão 27: 
somaListaCauda :: [Int] -> Int
somaListaCauda [] = 0
somaListaCauda (a:b) = somaListaAux a b

somaListaAux :: Int -> [Int] -> Int
somaListaAux acc [] = acc
somaListaAux acc (a:b) = somaListaAux (acc+a) b

--Questão 28:

--A) 2
--B) 1
--C) 2
--D) 1
--E) 0
--F) 1
--G) 2

--Questão 29:

type Matricula = Int
type Nome = String
type Titulacao = String
type Sexo = Char
type Bd = [(Matricula,Nome,Titulacao,Sexo)]

banco :: Int -> (Matricula,Nome,Titulacao,Sexo)
banco matricula
  |matricula == 1 = (1,"Roque","Doutor",'M')
  |matricula == 2 = (2,"Alzira","Doutor",'F')
  |matricula == 3 = (3,"Helio","Doutor",'M')
  |matricula == 4 = (4,"Maisa","Doutor",'F')
  |matricula == 5 = (5,"Carlos","Mestre",'M')
  |matricula == 6 = (6,"Rita","Mestre",'F')
  |otherwise = (0,"","",' ')

--A)
criarbd :: [Int] -> Bd
criarbd [] = []
criarbd (a:b) = banco a : criarbd b

obterDoutoresAux :: Bd -> Bd
obterDoutoresAux [] = []
obterDoutoresAux ((m,n,t,s):b)
  | t == "Doutor" = (m,n,t,s) : obterDoutoresAux b
  | otherwise = obterDoutoresAux b

obterDoutores :: Int
obterDoutores = contaElements (obterDoutoresAux (criarbd [1..6]))

contaElements :: [(Matricula,Nome,Titulacao,Sexo)] -> Int
contaElements [] = 0
contaElements (a:b) = 1 + contaElements b

--B)
obterMulheresAux :: Bd -> Bd
obterMulheresAux [] = []
obterMulheresAux ((m,n,t,s):b)
  | s == 'F' = (m,n,t,s) : obterMulheresAux b
  | otherwise = obterMulheresAux b

obterMulheres :: Int
obterMulheres = contaElements (obterMulheresAux (criarbd [1..6]))

--C)
obterMulheresMestres :: Int
obterMulheresMestres = contaElements (obterMulheresMestresAux (obterMulheresAux (criarbd [1..6])))

obterMulheresMestresAux :: Bd -> Bd
obterMulheresMestresAux [] = []
obterMulheresMestresAux ((m,n,t,s):b)
  | t == "Mestre" = (m,n,t,s) : obterMulheresMestresAux b
  | otherwise = obterMulheresMestresAux b

--D)
obterNomeDoutores :: [String]
obterNomeDoutores = obterNomeDoutoresAux (obterDoutoresAux (criarbd [1..6]))

obterNomeDoutoresAux :: Bd -> [String]
obterNomeDoutoresAux [] = []
obterNomeDoutoresAux ((m,n,t,s):b) = n : obterNomeDoutoresAux b

------------------------------------------------------------------------------------

-- Funções auxiliares:

--Função Split:
pegaPalavra :: String -> String
pegaPalavra [] = []
pegaPalavra (a:b)
  | a == ' ' = ""
  | otherwise = a:pegaPalavra b

discartaPalavra :: String -> String
discartaPalavra [] = []
discartaPalavra (a:b)
  | a == ' ' = b
  | otherwise = discartaPalavra b

split :: String -> [String]
split [] = []
split str = pegaPalavra str : split (discartaPalavra str)
--Pode se usar (a:b) (a:b:c) (a:b:c:d) para pegar 1, 2 ou 3 palavras respectivamente ou usar str para pegar todas as palavras

--Pegar inicial:
pegaInicial :: String -> String
pegaInicial [] = []
pegaInicial (a:b) = [chr (ord a + 32)]

--concatenaInicial
concatenaIni :: [String] -> String
concatenaIni [] = []
concatenaIni (a:b) = pegaInicial a ++ concatenaIni b

geraEmail :: String -> String
geraEmail str = concatenaIni (split str) ++ "@uesb.edu.br"

--Convertendo Lista de Nomes em lista de emails
--Remover palavras "do", "dos", "de",...
--Verificar pessoas com mesmas Iniciais
--rss e rss viram rss e rss2
--Verifica tamanho e espaços
--Precisa ter nome e sobrenome


--Converte número em String:
unidades :: [String]
unidades = ["zero", "um", "dois", "três", "quatro", "cinco", "seis", "sete", "oito", "nove"]

dezenas :: [String]
dezenas = ["vinte", "trinta", "quarenta", "cinquenta", "sessenta", "setenta", "oitenta", "noventa"]

dezenasEspeciais :: [String]
dezenasEspeciais = ["dez", "onze", "doze", "treze", "quatorze", "quinze", "dezesseis", "dezessete", "dezoito", "dezenove"]

centenas :: [String]
centenas = ["cento", "duzentos", "trezentos", "quatrocentos", "quinhentos", "seiscentos", "setecentos", "oitocentos", "novecentos"]

milhares :: [String]
milhares = ["mil", "milhão", "milhão", "bilhão", "bilhão", "trilhão"]

converte :: Int -> String
converte numero = tratar (dezena, unidade)
  where
    milhar = div numero 1000
    centena = div numero 100
    dezena = div numero 10
    unidade = mod numero 10

tratar :: (Int,Int) -> String
tratar (0, u) = unidades !! u -- O Operador retorna !! retorna o elemento daquela lista
tratar (1, u) = dezenasEspeciais !! u
tratar (d, 0) = dezenas !! (d - 2)
tratar (d, u) = dezenas !! (d-2) ++ " e " ++ unidades !! u

--Sistema de biblioteca:

type Usuario = String
type Usuarios = [Usuario]

type Livro = String
type Livros = [Livro]

type Emprestimo = (Usuario, Livro)
type Emprestimos = [Emprestimo]

emprestimos :: Emprestimos
emprestimos = [("João", "Livro 1"), ("Maria", "Livro 2"), ("José", "Livro 3"), ("Ana", "Livro 4"), ("Maria", "Livro 5")]

usuarios :: Usuarios
usuarios = ["João", "Maria", "José", "Ana", "Carlos"]

livros :: Livros
livros = ["Livro 1", "Livro 2", "Livro 3", "Livro 4", "Livro 5"]

obterLivros :: Usuario -> Emprestimos -> Livros
obterLivros usuario [] = []
--Como a cabeça vai ser um tupla podemos especificar cada elemento do par usando (n,l)
obterLivros usuario ((n,l):b)
  | usuario == n = l : obterLivros usuario b
  | otherwise = obterLivros usuario b

obterUsuarios :: Livro -> Emprestimos -> Usuarios
obterUsuarios livro [] = []
obterUsuarios livro ((n,l):b)
  | livro == l = n : obterUsuarios livro b
  | otherwise = obterUsuarios livro b

devolucao :: Livro -> Usuario -> Emprestimos -> Emprestimos
devolucao livro usuario [] = error "Registro não encontrado na base de dados"
devolucao livro usuario ((n,l):b)
  | n == usuario && l == livro = b
  | otherwise = (n,l) : devolucao livro usuario b