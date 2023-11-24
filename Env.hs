module Env where

import Types (Binding (..), Env, Value)

-- | Ambiente vazio.
--
-- Representa um ambiente sem valores. Como o ambiente
-- é implementado usando uma lista, um ambiente vazio é
-- nada mais que uma lista vazia.
emptyEnv :: Env
emptyEnv = []

-- | Adiciona um vínculo nome-valor a um ambiente.
--
-- Esta função é responsável por adicionar valores a um ambiente.
-- Como ambientes são listas ligadas, a função `extendEnv` é
-- equivalente à função _cons_ `:`, que adiciona um elemento ao
-- início de uma lista ligada.
--
-- Por exemplo, as seguintes construções são equivalentes:
--
-- >>> import Types (Value (NumV))
-- >>> extendEnv (Binding "x" (NumV 1)) emptyEnv
-- [Binding {nameB = "x", valueB = NumV {numV = 1.0}}]
--
-- >>> (Binding "x" (NumV 1)) : []
-- [Binding {nameB = "x", valueB = NumV {numV = 1.0}}]
extendEnv :: Binding -> Env -> Env
extendEnv = (:)

-- | Procura um valor em um ambinte pelo seu identificador (nome).
--
-- Esta função é nada mais que uma busca em lista ligada.
-- Caso o ambiente fosse implementado usando outra estrutura de dados,
-- poderíamos realizar esta busca com algoritmos mais rápidos. Um
-- exercício opcional interessante é mudar a implementação de `Env`
-- para ser equivalente a `Data.Map String Value`.
lookupEnv :: String -> Env -> Value
lookupEnv name env = case env of
  [] -> error ("ERRO lookupEnv: não foi possível encontrar " ++ name ++ " no ambiente.")
  (binding : rest) ->
    if name == nameB binding
      then valueB binding
      else lookupEnv name rest
