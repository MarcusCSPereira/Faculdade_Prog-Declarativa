module Arquivos where

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