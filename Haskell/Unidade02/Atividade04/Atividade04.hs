{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use print" #-}


module Atividade04 where

-- Q_1:

data OP = SOMA | SUB | PROD | DIV deriving (Show, Eq)

data Expr = Folha Int | Nodo OP Expr Expr deriving (Show, Eq)

-- A)
aplica :: OP -> Int -> Int -> Int
aplica SOMA x y = x + y
aplica SUB x y = x - y
aplica PROD x y = x * y
aplica DIV x y = x `div` y
aplica _ x y = error "Operação inválida"

-- B)
avalia :: Expr -> Int
avalia (Folha x) = x
avalia (Nodo op e1 e2) = aplica op (avalia e1) (avalia e2)

-- C)
imprime :: Expr -> String
imprime (Folha x) = show x
imprime (Nodo op e1 e2) = "(" ++ imprime e1 ++ show op ++ imprime e2 ++ ")"

-- Q_2:

data Part = AM | PM deriving (Show, Eq)
data TIME = Local Int Int Part | Total Int Int

time :: TIME
time = Local 12 30 PM

-- A)

totalMinutos :: TIME -> Int
totalMinutos (Local h m AM) = h * 60 + m
totalMinutos (Local h m PM) = (h + 12) * 60 + m
totalMinutos (Total h m) = h * 60 + m

-- B)
instance Eq TIME where
  (==) :: TIME -> TIME -> Bool
  t1 == t2 = totalMinutos t1 == totalMinutos t2

-- C)
instance Ord TIME where
  compare :: TIME -> TIME -> Ordering
  compare t1 t2 = compare (totalMinutos t1) (totalMinutos t2)

-- Q_3:
data Nat = Zero | Succ Nat

-- A)
natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

-- B)
soma :: Nat -> Nat -> Nat
soma Zero Zero = Zero
soma Zero n = n
soma n Zero = n
soma (Succ n1) n2 = Succ (soma n1 n2)

-- C)
instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  Zero == Zero = True
  Zero == _ = False
  _ == Zero = False
  (Succ n1) == (Succ n2) = n1 == n2

instance Show Nat where
  show :: Nat -> String
  show n = show (natToInt n)

-- 4 e 5 no caderno

--6) 
class Calculavel a where
  area :: a -> Double
  perimetro :: a -> Double
  descricao :: a -> String

--a)

data Circulo = Circulo Double

--b)
instance Calculavel Circulo where
  area (Circulo r) = pi * r * r
  perimetro (Circulo r) = 2 * pi * r
  descricao (Circulo r) = "Circulo de raio " ++ show r

--c)
data Retangulo = Retangulo Double Double
data Quadrado = Quadrado Double

instance Calculavel Retangulo where
  area (Retangulo b h) = b * h
  perimetro (Retangulo b h) = 2 * b + 2 * h
  descricao (Retangulo b h) = "Retangulo de base " ++ show b ++ " e altura " ++ show h

instance Calculavel Quadrado where
  area (Quadrado l) = l * l
  perimetro (Quadrado l) = 4 * l
  descricao (Quadrado l) = "Quadrado de lado " ++ show l

--7)
main1 :: IO ()
main1 = do
  conteudo <- readFile "entrada.txt"
  let linhas = contarLinhas conteudo
  let palavras = contarPalavras conteudo
  let caracteres = contarCaracteres conteudo
  putStrLn  ("Linhas: " ++ show linhas ++ "\n" ++ "Palavras: " ++ show palavras ++ "\n" ++ "Caracteres: " ++ show caracteres)


contarLinhas :: String -> Int
contarLinhas [] = 0
contarLinhas [_] = 1
contarLinhas (x:xs)
    | x == '\n' = 1 + contarLinhas xs
    | otherwise = contarLinhas xs

contarPalavras :: String -> Int
contarPalavras [] = 0
contarPalavras (x:xs)
    | isSpace x = contarPalavras xs
    | otherwise = 1 + contarPalavras (pularPalavra xs)

pularPalavra :: String -> String
pularPalavra [] = []
pularPalavra (x:xs)
    | isSpace x = xs
    | otherwise = pularPalavra xs

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace '\n' = True
isSpace '\t' = True
isSpace _ = False

contarCaracteres :: String -> Int
contarCaracteres [] = 0
contarCaracteres (x:xs)
    | isSpace x = contarCaracteres xs
    | otherwise = 1 + contarCaracteres xs

--8)

main2 :: IO ()
main2 = do
  let numeroSecreto = 42
  putStrLn "Adivinhe o número secreto entre 1 e 100"
  adivinhar numeroSecreto 0

adivinhar :: Int -> Int -> IO ()
adivinhar numeroSecreto tentativas = do
  putStrLn "Digite um número:"
  numero <- getLine
  let numeroInt = read numero :: Int
  if numeroInt == numeroSecreto
    then putStrLn ("Parabéns! Você acertou em " ++ show tentativas ++ " tentativas")
    else do
      if numeroInt < numeroSecreto
        then putStrLn "O número secreto é maior"
        else putStrLn "O número secreto é menor"
      adivinhar numeroSecreto (tentativas + 1)

--9)
-- Função principal para iniciar a agenda
main3 :: IO ()
main3 = agenda []

-- Função da agenda que exibe o menu e processa a escolha do usuário
agenda :: [(String, String)] -> IO ()
agenda contatos = do
    putStrLn "\n--- Menu da Agenda ---"
    putStrLn "1. Incluir contato"
    putStrLn "2. Pesquisar contato"
    putStrLn "3. Listar contatos"
    putStrLn "4. Excluir contato"
    putStrLn "5. Sair"
    putStr "Escolha uma opção: "
    opcao <- getLine
    case opcao of
        "1" -> incluirContato contatos
        "2" -> pesquisarContato contatos
        "3" -> listarContatos contatos
        "4" -> excluirContato contatos
        "5" -> putStrLn "Encerrando a agenda."
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            agenda contatos

-- Função para incluir um novo contato
incluirContato :: [(String, String)] -> IO ()
incluirContato contatos = do
    putStr "Digite o nome: "
    nome <- getLine
    putStr "Digite o telefone: "
    telefone <- getLine
    let novoContato = (nome, telefone)
    putStrLn ("Contato adicionado: " ++ show novoContato)
    agenda (novoContato : contatos)

-- Função para pesquisar um contato pelo nome
pesquisarContato :: [(String, String)] -> IO ()
pesquisarContato contatos = do
    putStr "Digite o nome a ser pesquisado: "
    nome <- getLine
    let resultados = filter (\(n, _) -> n == nome) contatos
    if null resultados
        then putStrLn "Contato não encontrado."
        else putStrLn ("Contatos encontrados: " ++ show resultados)
    agenda contatos

-- Função para listar todos os contatos sem usar mapM_
listarContatos :: [(String, String)] -> IO ()
listarContatos [] = do
    putStrLn "A agenda está vazia."
    agenda []
listarContatos contatos = do
    putStrLn "Contatos na agenda:"
    listarContatosRecursivo contatos
    agenda contatos

-- Função recursiva auxiliar para imprimir cada contato
listarContatosRecursivo :: [(String, String)] -> IO ()
listarContatosRecursivo [] = return ()
listarContatosRecursivo (contato:resto) = do
    putStrLn (show contato)
    listarContatosRecursivo resto

-- Função para excluir um contato pelo nome
excluirContato :: [(String, String)] -> IO ()
excluirContato contatos = do
    putStr "Digite o nome do contato a ser excluído: "
    nome <- getLine
    let novosContatos = filter (\(n, _) -> n /= nome) contatos
    if length novosContatos == length contatos
        then putStrLn "Contato não encontrado."
        else putStrLn "Contato excluído com sucesso."
    agenda novosContatos