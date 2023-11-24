module Interpreter where

import Tokenizer (tokenize)
import Parser (analyze, parse, parseSequence)
import Desugar (desugar)
import Evaluator (eval)
import Env (emptyEnv)
import Types (Env, Value)

-- | Interpretador.
--
-- Esta função recebe um código da linguagem a ser interpretada,
-- um ambiente no qual o interpretador irá buscar por valores, e
-- retorna um valor primitivo da linguagem. Como programas
-- normalmente começam com um ambiente vazio, também foi
-- disponibilizada a função `interp0`, idêntica a `interp`, mas que
-- utiliza um ambiente vazio.
--
-- Lembre que o operador '.' em Haskell é responsável pela _composição_
-- de funções: @f . g@ é equivalente a @f (g x)@. Este operador é o
-- mesmo utilizado na matemática: f . g representa f(g(x)).
--
-- Observando o código desta função, vemos que o processo de
-- interpretação pode ser visto como a composição da _avaliação_,
-- _desaçucarização_, _análise semântica_, _parsing_, e _tokenização_.
-- Lembre que na composição f . g, g é executado antes de f. Desta
-- forma, a interpretação realizará primeiro a tokenização, depois o
-- parsing, depois a análise semântica, depois a desaçucarização, e
-- por último a avaliação.
--
-- Para entender o funcionamento de cada parte, veja a documentação
-- de sua respectiva implementação. É recomendado estudar cada etapa
-- conforme a ordem de execução da interpretação.
interp :: String -> Env -> Value
interp = eval . desugar . analyze . parse . tokenize

-- | Interpretação com ambiente vazio.
--
-- Realiza a interpretação de um determinado código utilizando um
-- ambiente vazio como inicial. É a principal função a se utilizar
-- quando testando a linguagem.
interp0 :: String -> Value
interp0 code = interp code emptyEnv

-- | Interpretação de múltiplas expressões de código.
--
-- Nesta versão da interpretação, o parsing resultará em múltiplas
-- árvores sintáticas que devem ser tratadas individualmente. Não
-- é necessário usar esta função.
interpMany :: String -> Env -> [Value]
interpMany code env =
  [(eval . desugar . analyze) tree env | tree <- (parseSequence . tokenize) code]
