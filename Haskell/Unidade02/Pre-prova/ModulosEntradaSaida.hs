{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Redundant return" #-}
module ModulosEntradaSaida where

--Import do modulo para gerar numeros aleatorios
import System.Random (randomRIO)
--Usando entrada e saída

main :: IO ()
main = do putStrLn "Digite seu nome: "
          nome <- getLine
          putStrLn ("Olá, " ++ nome ++ "!")
          putStrLn "Digite sua idade: "
          idade <- readLn :: IO Int
          putStr ("Sua idade é: ")
          print idade

lerNumero :: IO Int
lerNumero = do putStrLn "Digite um numero"
               n <- readLn
               return n

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n-1)

main2:: IO ()
main2 =do n1<-lerNumero
          print("Resultado: " ++ show (fatorial n1))
          putStrLn "Quer Continuar? (y/n)"
          resposta <- getLine
          case resposta of
            "y" -> main2
            "Y" -> main2
            "n" -> return ()
            _ -> putStrLn "Resposta invalida" >> main2


--Exemplo de sorteio:

jogo:: IO String
jogo  = do
        putStrLn "Vamos jogar um jogo. Adivinhe um numero de 1 a 100"
        numeroAlvo <- randomRIO (1,100)
        jogoDosAleatorios' numeroAlvo 0


jogoDosAleatorios' :: Int -> Int -> IO String
jogoDosAleatorios' numeroAlvo  tentativas = do
                                           putStr "Seu chute: "
                                           input <- getLine
                                           let chute = read input :: Int
                                           if chute == numeroAlvo then return ("Acertou! Tentativas: " ++ show (tentativas+1))
                                              else  do
                                                    if chute > numeroAlvo then putStrLn "O numero eh menor que esse"
                                                    else putStrLn "O numero eh maior que esse"
                                                    jogoDosAleatorios' numeroAlvo (tentativas + 1)
