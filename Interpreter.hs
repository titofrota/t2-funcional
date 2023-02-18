{- Programacao Funcional - Trabalho 2 - 
   Prazo de entrega: 19/02/2023 por Tarefa no Aprender 3
   O trabalho deve ser feito individualmente

  ** ENUNCIADO DO TRABALHO:   
 Evolua o interpretador abaixo para prover avaliacao preguicosa (lazy evaluation).
 Ao fim do codigo abaixo, ha alguns casos de testes. 
 Sugere-se observar as dicas neste arquivo e no AbsLI.hs.
 
 Criterios de avaliacao: 
   1) testCaseSuiteResults deve ser computavel e ser True 
   2) para nota maior ou igual a 9, deve-se usar SYB.

-}

module Interpreter where

import AbsLI
import Prelude 
import Data.Maybe
import Memo
import Data.Generics


type Context k v = [(k,v)]                                
type FContext = Context Ident Function
type EContext = Context Ident Exp
type LContext = (EContext, FContext)


evalP :: Program -> Integer
evalP (Prog fs) =  eval ([],(updatecF [] fs)) (Call (Ident "main") [])   

eval :: LContext -> Exp -> Integer
eval context x = case x of
    EAdd exp0 exp  -> eval context exp0  +  eval context exp
    ESub exp0 exp  -> eval context exp0  -  eval context exp
    EMul exp0 exp  -> eval context exp0  *  eval context exp
    EDiv exp0 exp  -> eval context exp0 `div` eval context exp
    EInt n         -> n
    EIf e1 e2 e3   -> if (eval context e1 /= 0) 
                        then (eval context e2) 
                        else (eval context e3)
    EVar id        -> eval context (fromJust (lookupMemo id (fst context)))
    Call id lexp   -> eval (paramBindings,contextFunctions) exp'
                          where 
                              (Fun _ args exp) = fromJust (lookupMemo id (snd context))
                              paramBindings = zip args lexp
                              contextFunctions = snd context
                              exp' = everywhere (mkT $ transformExp paramBindings) exp

transformExp :: EContext -> Exp -> Exp
transformExp ctx exp = everywhere (mkT $ replaceId) exp
    where
        replaceId :: Exp -> Exp
        replaceId (EVar id') =
            fromMaybe (EVar id') $ lookupMemo id' ctx >>= \newExp -> Just $ replaceId newExp
        replaceId e = e


updatecF :: FContext -> [Function] -> FContext
updatecF ctx [] = ctx
updatecF ctx (f@(Fun id _ _):fs) = updatecF (updateMemo ctx id f) fs


{-
  main () {
    fat (5)
  }
  
  fat (n) {
    if (n) 
       then n * fat (n - 1) 
       else 1
  }
-}

fat =  Prog [Fun (Ident "main") [] (Call (Ident "fat") [EInt 5]),
             Fun (Ident "fat") [Ident "n"] 
                                         (EIf (EVar (Ident "n")) 
                                           (EMul (EVar (Ident "n")) 
                                                  (Call (Ident "fat") [ESub (EVar (Ident "n")) (EInt 1)])) 
                                           (EInt 1))]

testCaseFat = evalP fat == 120


{-
 main () {
   fib (8)
}
 fib (n) {
   if (n) then 
      if (n - 1) 
        then fib (n - 1) + fib (n - 2) 
        else 1 
    else 1
}
-}

fibo = Prog [Fun (Ident "main") [] (Call (Ident "fib") [EInt 8]),
             Fun (Ident "fib") [Ident "n"] 
                        (EIf (EVar (Ident "n")) 
                                (EIf (ESub (EVar (Ident "n")) (EInt 1)) 
                                    (EAdd (Call (Ident "fib") [ESub (EVar (Ident "n")) (EInt 1)]) 
                                          (Call (Ident "fib") [ESub (EVar (Ident "n")) (EInt 2)])) 
                                    (EInt 1)) 
                              (EInt 1))]

testCaseFibo = evalP fibo == 34

add = Prog [Fun (Ident "main") [] (Call (Ident "add") [EInt 2, EInt 3]),
            Fun (Ident "add") [Ident "x", Ident "y"]
                      (EIf (EVar (Ident "y"))
                           (Call (Ident "add") [EAdd (EVar (Ident "x")) (EInt 1), ESub (EVar (Ident "y")) (EInt 1)])
                           (EVar (Ident "x")))
           ]

testCaseAdd = evalP add == 5


square = Prog [Fun (Ident "main") [] (Call (Ident "square") [EInt 5]),
               Fun (Ident "square") [Ident "n"]
                                    (EMul (EVar (Ident "n")) (EVar (Ident "n")))]

testCaseSquare = evalP square == 25

-- testCaseSuiteResults deve ser true 
testCaseSuiteResults  = testCaseFat && testCaseFibo && testCaseAdd && testCaseSquare

