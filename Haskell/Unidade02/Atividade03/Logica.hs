{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Logica where

lerNumero :: IO Int
lerNumero = do putStrLn "Digite um numero"
               n <- readLn
               return n

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n-1)