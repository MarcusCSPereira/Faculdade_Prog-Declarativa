{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module TipoAbstrato (Stack, emptyStack, push, pop, top, isEmpty) where 

--Tipo Abstrato:
data Stack t = Empty | Stack t (Stack t) deriving (Show,Eq)

emptyStack :: Stack t
emptyStack = Empty

push :: t -> Stack t -> Stack t
push x s = Stack x s

pop :: Stack t -> Stack t
pop Empty = error "pilha vazia irmão"
pop (Stack _ s) = s

top :: Stack t -> t
top Empty = error "pilha vazia irmão"
top (Stack x _) = x

isEmpty :: Stack t -> Bool
isEmpty Empty = True
isEmpty _ = False

