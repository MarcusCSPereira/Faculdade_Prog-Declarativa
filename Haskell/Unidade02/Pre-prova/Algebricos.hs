{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Algebricos where

--Tipo Enumerado:

data Boolean = Verdadeiro | Falso
    deriving (Show)

data Estacao = Verao | Outono | Inverno | Primavera
    deriving (Show)

--usando:
tempo :: Estacao -> String
tempo Verao = "Calor"
tempo _ = "Frio"

--Tipo Produto:
type Nome = String
type Preco = Float
data Casa = C Nome Preco deriving (Show)

--usando:
casa :: Casa
casa = C "Casa" 100000.0

nomeCasa :: Casa -> Nome
nomeCasa (C n p) = n

--Tipo Alternativa:

data Forma = Circulo Float | Retangulo Float Float
    deriving (Show)

--usando:
area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo b a) = b * a

--Tipo Recursivo:
data Stack t = Empty | T t (Stack t)
    deriving (Show)

--usando:
push :: t -> Stack t -> Stack t
push x s = T x s

pop :: Stack t -> Stack t
pop Empty = error "pilha vazia irmão"
pop (T _ s) = s

