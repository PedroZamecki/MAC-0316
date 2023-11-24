module Main where

import System.IO (hFlush, stdout)
import Control.Exception (SomeException (SomeException), try)
import Control.Exception.Base (evaluate)

import Interpreter (interp0)
import Types (Value)

-- | Função `main` do interpretador.
-- 
-- Chama a função `repl`, que define um loop de acesso ao interpretador.
-- 
-- Para entender como o interpretador funciona, comece sua jornada pela
-- função `interp` encontrada no arquivo `Interpreter.hs`.
main :: IO ()
main = repl

-- | Define um REPL para a função `interp0`.
--
-- REPL significa "Read, Evaluate, Print, Loop". É um tipo de
-- ambiente muito usado para interagir com linguagens de programação
-- de forma dinâmica. Ele interpreta, de forma dinâmica, o código que
-- um usuário escreva no prompt em cada linha.
-- 
-- Por exemplo, um REPL muito usado é o do Python. Caso tenha o Python
-- instalado em sua máquina, você pode acessar seu REPL digitando
-- `python3` em uma janela de terminal.
-- 
-- Esta é a única função do projeto que lida com input/output externo.
-- Haskell, por ser uma linguagem de programação com avaliação por
-- demanda, precisa encapsular seus acessos a input/output externo em
-- uma camada de abstração (no caso, o tipo IO). Para acessar o valor
-- resgatado por uma função que retorna IO, é necessário usar uma sintaxe
-- especial.
-- 
-- Veja, por exemplo, a linha `line <- getLine`. Acessando a documentação
-- da função getLine, você pode ver que ela retorna algo do tipo `IO String`.
-- Mas, a nossa função de interpretação `interp0` recebe algo do tipo
-- `String`, não algo do tipo `IO String`!
-- 
-- Para resolver este problema, precisamos desencapsular a `String` de dentro
-- do `IO`. Fazemos isto usando a _do notation_ e a flecha `<-`. A _do
-- notation_ é um _açúcar sintático_ de Haskell que facilita o uso de Monads, 
-- como o IO. Para este exercíco, basta saber que a sintaxe `line <- getLine` 
-- permite que você desencapsule a `String` de `IO String` em um novo valor 
-- chamado `line`. Para entender como isto é feito debaixo dos panos, é preciso
-- saber mais sobre Monads.
-- 
-- Quem tiver interesse em entender Monads, pode dar uma olhada em
-- <https://wiki.haskell.org/All_About_Monads>
repl :: IO ()
repl = do -- `do` nos avisa que estamos dentro de um bloco de _do notation_
  putStr ">>> "   -- escreve ">>> " no terminal
  hFlush stdout   -- envia o que foi escrito ao `stdout`
  line <- getLine -- lê uma linha do terminal
  case line of
    "quit" -> return () -- termina o programa
    "" -> repl          -- lê uma linha novamente (note a recursão!)
    _ -> do             -- avalia a linha escrita, tratando exceções
      -- a função `try` transforma exceções no tipo Either
      result <- try (evaluate (interp0 line)) :: IO (Either SomeException Value)
      case result of
        Left exception -> print exception
        Right val -> print val
      repl
