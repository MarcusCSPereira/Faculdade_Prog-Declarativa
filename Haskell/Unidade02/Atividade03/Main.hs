{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unless" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant return" #-}
module Main(main) where

import Logica (lerNumero, fatorial)
import Control.Monad.RWS (MonadState(put))

main :: IO ()
main = do n1<-lerNumero
          print("Resultado: " ++ show (fatorial n1))
          putStrLn "Quer Continuar? (y/n)"
          resposta <- getLine
          case resposta of
            "y" -> main
            "Y" -> main
            "n" -> return ()
            _ -> putStrLn "Resposta invalida"