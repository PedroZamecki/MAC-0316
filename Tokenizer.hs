module Tokenizer where

import Types (Token)

-- | O Tokenizer transforma um texto da linguagem em uma lista de tokens.
-- 
-- Os tokens representam os símbolos da linguagem, e posteriormente serão utilizados
-- para transformar o código fonte em uma _árvore sintática_ no processo de parsing.
-- 
-- Por exemplo, o tokenizer transformará a string "(+ 10 20)" em uma lista de símbolos
-- igual a ["(", "+", "10", "20", ")"], de forma a separar os "átomos" de um código.
-- 
-- >>> tokenize "(+ 10 20)"
-- ["(","+","10","20",")"]
--
-- Como podemos ver no arquivo `Types.hs`, um Token é definido como um sinônimo
-- para String, isto é, os tipos `Token` e `String` são equivalentes.
-- Em compiladores mais avançados, o tokenizer é substituído por algo que chamamos
-- de _Lexer_, que além de realizar o processo de tokenização, também irá marcar
-- cada token com o que o token representa. No exemplo "(+ 10 20)", os números
-- poderiam ser marcados pelo Lexer como sendo números; no nosso interpretador,
-- são apenas Strings.
tokenize :: String -> [Token]
tokenize [] = []
tokenize str@(char : chars)
  -- ^ a sintaxe `lista@(cabeça : cauda)` nos permite desestruturar a
  -- a lista ao mesmo tempo que damos um nome a ela
  | char == '(' || char == ')' = [char] : tokenize chars
  | isNumeralChar char = accumulate isNumeralChar
  | isSymbolChar char = accumulate isSymbolChar
  | otherwise = tokenize chars
  where
    -- Caso tenhamos um token com múltiplos caracteres, a função
    -- `accumulate` nos permite acumulá-los e continuar a tokenização
    -- desconsiderando estes caracteres.
    accumulate f =
      let token = getToken f str
       in token : tokenize (drop (length token) str)

-- | Enquanto um predicado for verdadeiro, a função `getToken` acumula
-- múltiplos caracteres em um só token.
-- 
-- Lembrando que _predicados_ podem ser vistos como funções que retornam
-- valores booleanos (verdadeiro ou falso).
getToken :: (Char -> Bool) -> String -> Token
getToken _ [] = []
getToken pred (char : chars)
  | pred char = char : getToken pred chars
  | otherwise = []

-- | Lista de todos os caractes válidos para símbolos da linguagem.
-- 
-- Lembrando que uma `String` é uma lista de `Char`.
symbols :: String
symbols = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_!?-+*/%<>#~"

-- | Lista de todos os digitos válidos para números da linguagem.
numerals :: String
numerals = "0123456789."

-- | Predicado para caracteres de símbolos.
-- 
-- Note que este código usa _currying_ na função `elem`.
-- `elem` verifica se um valor pode ser encontrado em uma coleção.
-- O currying é feito na coleção `symbols`, efetivamente fixando-a 
-- e criando uma nova função que só recebe o caracter a ser buscado na coleção.
isSymbolChar :: Char -> Bool
isSymbolChar = (`elem` symbols)

-- | Predicado para símbolos.
-- 
-- Verifica se uma String é composta somente por caracteres válidos de símbolos.
isSymbol :: String -> Bool
isSymbol = all isSymbolChar

-- | Predicado para caracteres de números.
isNumeralChar :: Char -> Bool
isNumeralChar = (`elem` numerals)

-- | Predicado para caracteres de números.
-- 
-- Verifica se uma String é composta somente por caracteres válidos de números.
isNumeral :: String -> Bool
isNumeral = all isNumeralChar
