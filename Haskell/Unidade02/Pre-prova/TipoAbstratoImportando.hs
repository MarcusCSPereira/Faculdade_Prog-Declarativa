{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module TipoAbstratoImportando where

import TipoAbstrato (Stack, emptyStack, push, pop, top, isEmpty)

--Usando isso

stack :: Stack Int
stack = push 1 (push 2 (push 3 emptyStack))

--topo da pilha
topo :: Int
topo = top stack

--retirando o topo da pilha
pop' :: Stack Int -> Stack Int
pop' s = pop s






