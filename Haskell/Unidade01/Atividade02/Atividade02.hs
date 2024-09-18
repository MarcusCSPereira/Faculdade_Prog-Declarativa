module Atividade02 where
import Data.Char

{-
  Transformar função em operador use `mod`, e para transformar operador em função crie ele como uma função usando (operador) :: Int -> Int
-}

-- Q_01
somaPar :: Int -> Int
somaPar 0 = 0
somaPar n 
  | mod n 2 == 0 = n + somaPar (n-1)
  | otherwise = somaPar (n-1)

--Q_02
exponencial :: Integer -> Integer -> Integer
exponencial x 0 = 1
exponencial x y = x * (exponencial x (y-1))

--Q_03
somaIntervalo :: Int -> Int -> Int
somaIntervalo x y
  | x == y = x
  | otherwise = (somaIntervalo x (y-1)) + y

--Q_04
somaDigitos :: Int -> Int
somaDigitos 0 = 0
somaDigitos n = (mod n 10) + somaDigitos (div n 10)

--Q_05
somaQuadrados :: Integer -> Integer
somaQuadrados 0 = 1
somaQuadrados x = somaQuadrados (x-1) + (exponencial x 2)

--Q_06
somaFatoriais :: Integer -> Integer
somaFatoriais 1 = 1
somaFatoriais x
  | x < 0 = error "Valor inválido"
  | otherwise = somaFatoriais(x-1) + fatorial (x-1)

fatorial :: Integer -> Integer
fatorial n = fatAux n 1

fatAux :: Integer -> Integer -> Integer
fatAux 0 acc = acc
fatAux n acc = fatAux (n-1) (n*acc)

--Q_7:

aproxxPi :: Integer -> Double
aproxxPi 0 = calculo 0
aproxxPi x = aproxxPi (x-1) + calculo x

calculo :: Integer -> Double
calculo x = 4.0 * (((-1) ** n) / (2*n + 1))
          where n = (fromIntegral x :: Double)

--Q_8:
somaDois :: Int -> Int -> Int
somaDois x 0 = x
somaDois x y = somaDois (succ x) (y-1)

--Q_9:
multiplica :: Int -> Int -> Int
multiplica x 0 = 0
multiplica x 1 = x
multiplica x y 
  | x < 0 && y < 0 = multiplica ((-1)*x) (y+1) + (x*(-1))
  | y < 0 = multiplicaAux x y * (-1)
  | otherwise = multiplica x (y-1) + x

multiplicaAux :: Int -> Int -> Int
multiplicaAux x 0 = 0
multiplicaAux x 1 = x
multiplicaAux x y = multiplicaAux x (y+1) + x

--Q_10:
contaDigitos :: Int -> Int -> Int
contaDigitos x y
  | x `mod` 10 == y = 1 + contaDigitos(x `div` 10) y
  | x `div` 10 == 0 && x `mod` 10 /= y = 0 
  | otherwise = contaDigitos(x `div` 10) y 

--Q_11:
decimalBinario :: Int -> Int
decimalBinario 0 = 0
decimalBinario x = (x `mod` 2) + 10 * decimalBinario (x `div` 2)

--Q_12:
concateNxM :: Int -> [Char] -> [Char]
concateNxM 0 msg = ""
concateNxM x msg = msg ++ concateNxM (x-1) msg

--Q_13: 
{-
  A recursão com cauda é uma recursão de tipo espcial, onde utilizamos uma função auxiliar recursiva, que calcula as operações enquanto chama os passos recursivos, na recursão com cauda o ultimo passo a ser realizado é a chamada recursiva.

  GPT: Recursão de cauda, ou tail recursion, é um tipo específico de recursão em que a chamada recursiva é a última operação realizada dentro da função. Em outras palavras, a função retorna diretamente o resultado da chamada recursiva sem realizar mais nenhuma operação depois disso.
  Quando uma função é recursiva de cauda, não é necessário manter o estado anterior da função na pilha de chamadas, o que permite que compiladores ou interpretadores otimizem a recursão e reutilizem a mesma estrutura de chamada (como se fosse uma iteração), economizando memória e prevenindo estouros de pilha.
-}

--Q_14: Para ver o tempo de execução utiliza :time e o nome da função com seu valor
fibonacci :: Int -> Int
fibonacci n = fibonacciAux n 0 1
  where
    fibonacciAux 0 acc _ = acc
    fibonacciAux n acc prox = fibonacciAux (n-1) prox (acc+prox)

--Q_15:
mdc :: Int -> Int -> Int
mdc a b
  | a < b || b < 0 || a < 0 = error "Valores inválidos"
  | b == 0 = a
  | otherwise = mdc b (mod a b)

--Q_16: Aqui usamos list comprehension
tabuada :: Int -> IO()
tabuada n = putStrLn (concat [imprimeLinhadaTabuada n b (n * b) ++ "\n" | b <- lista])
  where 
    lista = [1..10]

imprimeLinhadaTabuada :: Int -> Int -> Int -> String
imprimeLinhadaTabuada a b c = show a ++ " x " ++ show b ++ " = " ++ show c

--Q_17:
calcularE :: Integer -> Double
calcularE 0 = 1
calcularE n = sum [1 / fromIntegral (fatorial i) | i <- [0..n]]

--Q_18:
tabelaE :: Integer -> [(Integer, Double)]
tabelaE t = [(n, calcularE n) | n <- [1..t]]

imprimirTabelaE :: Integer -> IO ()
imprimirTabelaE t = mapM_ (\(n, eVal) -> putStrLn ("Termos: " ++ show n ++ ", e ≈ " ++ show eVal)) (tabelaE t)

--Q_19:
taylorSin :: Double -> Integer -> Double
taylorSin x 0 = x
taylorSin x n = taylorSin x (n - 1) + (-1) ** fromIntegral n * x ** fromIntegral (2*n+1) / fromIntegral (fatorial (2*n+1))

--Q_20:
taylorCos :: Double -> Integer -> Double
taylorCos x 0 = 1
taylorCos x n = taylorCos x (n - 1) + (-1) ** fromIntegral n * x ** fromIntegral (2*n) / fromIntegral (fatorial (2*n))



--Q_21: Relatório de Vendas

tamanhoLinha :: Int
tamanhoLinha = 30

-- Função que centraliza uma string dentro de um tamanho específico
centralizar :: Int -> String -> String
centralizar largura str =
    let espacos = (largura - length str) `div` 2
    in replicate espacos ' ' ++ str ++ replicate espacos ' '

-- Função para calcular a média das vendas
mediaVendas :: Int -> Double
mediaVendas n = fromIntegral (somavendas n) / fromIntegral (n + 1)

-- Função para calcular o desvio padrão das vendas
desvioPadraoVendas :: Int -> Double
desvioPadraoVendas n = sqrt (sum [(fromIntegral (vendas i) - media) ^ 2 | i <- [0..n]] / fromIntegral (n + 1))
  where
    media = mediaVendas n

-- Função para imprimir a média e o desvio padrão no relatório
imprimirMediaDesvio :: Int -> String
imprimirMediaDesvio n = centralizar tamanhoLinha ("Média = " ++ show (mediaVendas n)) ++ "\n" 
                        ++ centralizar tamanhoLinha ("Desvio Padrão = " ++ show (desvioPadraoVendas n))

-- Função que imprime a soma total das vendas
imprimirSoma :: Int -> String
imprimirSoma n = centralizar tamanhoLinha ("Soma = " ++ show(somavendas n))

-- Função que imprime a maior venda
imprimirMaior :: Int -> String
imprimirMaior n = centralizar tamanhoLinha ("Maior = " ++ show (maiorVenda n))

-- Função que calcula a maior venda
maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0
maiorVenda n = max (vendas n) (maiorVenda (n-1))

-- Função que imprime o cabeçalho do relatório
cabecalho :: String
cabecalho = centralizar tamanhoLinha "Relatorio de Vendas" ++ "\n" ++ impSimbolo tamanhoLinha "-" ++ "\n"

-- Função que imprime todas as linhas de vendas
imprimirLinhas :: Int -> String
imprimirLinhas 0 = centralizar tamanhoLinha (imprimirLinha 0)
imprimirLinhas n = imprimirLinhas (n-1) ++ "\n" ++ centralizar tamanhoLinha (imprimirLinha n)

-- Função que imprime a venda de um determinado dia
imprimirLinha :: Int -> String
imprimirLinha n = dia n ++ "\t" ++ show (vendas n)

-- Função que imprime o rodapé, incluindo soma, maior venda, média e desvio padrão
rodape :: Int -> String
rodape n = impSimbolo tamanhoLinha "-" ++ "\n" 
           ++ imprimirSoma n ++ "\n" 
           ++ imprimirMaior n ++ "\n" 
           ++ imprimirMediaDesvio n

-- Função que imprime os símbolos para criar linhas de separação
impSimbolo :: Int -> String -> String
impSimbolo 0 _ = ""
impSimbolo n s = s ++ impSimbolo (n-1) s

-- Função principal que imprime o relatório completo
relatorio :: Int -> IO ()
relatorio n = putStrLn (cabecalho ++ imprimirLinhas n ++ "\n" ++ rodape n)

-- Função para obter o nome do dia da semana
dia :: Int -> String
dia 0 = "Segunda"
dia 1 = "Terca"
dia 2 = "Quarta"
dia 3 = "Quinta"
dia 4 = "Sexta"

-- Função para obter as vendas de um dia específico
vendas :: Int -> Int
vendas n
   | n == 0 = 23
   | n == 1 = 34
   | n == 2 = 58
   | n == 3 = 12
   | n == 4 = 56
   | otherwise = 0

-- Função que soma todas as vendas
somavendas :: Int -> Int
somavendas 0 = vendas 0
somavendas n = vendas n + somavendas (n-1)
