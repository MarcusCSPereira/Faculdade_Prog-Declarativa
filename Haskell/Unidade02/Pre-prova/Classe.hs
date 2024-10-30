{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Classe where

--Classe em Haskell

--Definindo uma classe
class Stack t where
    emptyStack :: t
    push :: Int -> t -> t
    pop :: t -> t
    top :: t -> Int
    isEmpty :: t -> Bool

--Instanciando a classe
data Pilha = Empty | Stack Int Pilha

instance Stack Pilha where
    emptyStack = Empty
    push x s = Stack x s
    pop Empty = error "pilha vazia irmão"
    pop (Stack _ s) = s
    top Empty = error "pilha vazia irmão"
    top (Stack x _) = x
    isEmpty Empty = True
    isEmpty _ = False

--Definindo Eq para Pilha
instance Eq Pilha where
    (==) :: Pilha -> Pilha -> Bool
    Empty == Empty = True
    Stack x s == Stack x' s' = x == x' && s == s'
    _ == _ = False
    (/=) :: Pilha -> Pilha -> Bool
    Empty /= Empty = False
    Stack x s /= Stack x' s' = x /= x' || s /= s'
    _ /= _ = True

--Definindo Ord para Pilha
instance Ord Pilha where
    compare :: Pilha -> Pilha -> Ordering
    compare Empty Empty = EQ
    compare Empty _ = LT
    compare _ Empty = GT
    compare (Stack x s) (Stack x' s') = case compare x x' of
        EQ -> compare s s'
        x -> x

--Definindo Show para Pilha
instance Show Pilha where
    show :: Pilha -> String
    show Empty = "Empty"
    show (Stack x s) = show x ++ " " ++ show s

--Usando isso
stack :: Pilha
stack = push 1 (push 2 (push 3 emptyStack))

--Herança de classes
class Calculavel a where
    area :: a -> Float
    perimetro :: a -> Float

data Circulo = C Float

instance Calculavel Circulo where
    area (C r) = pi * r * r
    perimetro (C r) = 2 * pi * r





