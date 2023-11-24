module Parser where

import Text.Read (readMaybe)

import Tokenizer (isNumeralChar, isNumeral, isSymbol)
import Types (Token, ParseTree (..), ExprS (..))

-- | O parser transforma uma lista de tokens em uma árvora sintática.
-- Ele é responsável pela análise sintática da linguagem.
-- 
-- De forma resumida, a análise sintática utiliza as regras de sintaxe
-- de uma linguagem para transformar uma estrutura de texto em uma árvore
-- que pode ser mais facilmente processada pelo interpretador.
-- 
-- Para entender a estrutura da árvore sintática, veja a documentação de
-- `ParseTree` no módulo `Types.hs`.
--
-- Esta é uma das partes mais complicadas do processo de interpretação
-- e compilação de linguagens de programação. Há algumas abordagens possíveis
-- a serem tomadas - as mais populares são o uso de _parser generators_ e/ou
-- _parser combinators_. Parte da base teórica necessária para entender parser
-- generators é estudada na disciplina MAC0414 - Autômatos, Computabilidade e
-- Complexidade.
-- 
-- A implementação do interpretador não usa qualquer uma destas técnicas.
-- Não é necessário entendê-la completamente nem alterá-la, mas caso tenha
-- interesse, veja a função `parseToken`.
-- A função `parseSequence` é capaz de gerar mais de uma árvore sintática,
-- algo tratado como erro nesta versão. Efetivamente, isto permite processar
-- mais de uma linha de código de uma vez.
--
-- Exemplos de uso:
--
-- >>> import Tokenizer (tokenize)
-- >>> parse (tokenize "(+ 1 2)")
-- Pair (Leaf "+") (Pair (Leaf "1") (Pair (Leaf "2") Null))
--
-- O código acima representa a árvore
--
-- >   Pair
-- >  /   \
-- > +   Pair
-- >     /  \
-- >    1   Pair
-- >       /   \
-- >      2    Null
parse :: [Token] -> ParseTree
parse tokens
  | not (matchingParentheses tokens) = error "Erro parse: parêntesis não pareados corretamente"
  | not (null rest) = error ("ERRO parse: Parsing gerou mais de uma árvore sintática, " ++
                             "mais de uma linha de código deve ter sido processada")
  | otherwise = tree
  where
    (tree, rest) = parseToken tokens

-- | O analisador realiza análise semântica.
-- 
-- Esta etapa do interpretador atribui sentido (semântica) à árvore
-- sintática original. Cada token reconhecido como parte da linguagem
-- vai ser marcado com o que representa. Desta forma, a interpretação
-- do programa vai ser um simples algoritmo de travessia em árvore,
-- tratando cada elemento da árvore a partir de sua marcação.
-- 
-- A função retorna uma árvore do tipo `ExprS`, que representa uma
-- expressão da linguagem antes de ser _desaçucarizada_, isto é, antes
-- de ter seus açúcares sintáticos removidos. Para entender melhor o
-- processo de desaçucarização, veja a função `desugar` no arquivo
-- `Desugar.hs`.
-- 
-- Para adicionar funcionalidade à linguagem, é necessário acrescentar
-- código a esta função.
analyze :: ParseTree -> ExprS
analyze tree = case tree of
  Null -> error "ERRO analyze: input inválido"
  Leaf token
    | isNumeral token ->
      case readMaybe token of
        Just num -> NumS num
        Nothing -> error ("ERRO analyze: número inválido: " ++ token)
    | isSymbol token -> IdS token
    | otherwise -> error "ERRO analyze: token inválido"
  Pair first rest -> case first of
    Leaf "+" -> PlusS (analyzePos 1) (analyzePos 2)
    Leaf "*" -> MultS (analyzePos 1) (analyzePos 2)
    Leaf "-" -> BMinusS (analyzePos 1) (analyzePos 2)
    Leaf "~" -> UMinusS (analyzePos 1)
    Leaf "lambda" -> 
      let !id = getSymbol 1 
      in LamS id (analyzePos 2)
    Leaf "call" -> AppS (analyzePos 1) (analyzePos 2)
    Leaf "if"
      | invalidBool (getSymbol 1) -> error ("ERRO: booleano inválido: " ++ (getSymbol 1))
      | otherwise -> IfS (getSymbol 1) (analyzePos 2) (analyzePos 3)
    Leaf "cons" -> ConsS (analyzePos 1) (analyzePos 2)
    Leaf "head" -> HeadS (analyzePos 1)
    Leaf "tail" -> TailS (analyzePos 1)
    Leaf "let"  -> 
      let !id = getSymbol 1  
      in LetS id (analyzePos 2) (analyzePos 3)
    Leaf "let*" -> LetStarS (getSymbol 1) (analyzePos 2)
                            (getSymbol 3) (analyzePos 4)
                            (analyzePos 5)
    Leaf "letrec" -> 
      let !id = getSymbol 1
      in LetrecS id (analyzePos 2) (analyzePos 3)
    Leaf "quote"  -> QuoteS (show (tree `index` 1))
    _ -> error ("ERRO analyze: elemento da parse tree inesperado (" ++ show first ++ ")")
    where
      -- | Função auxiliar analisa a posição `i` da árvore recursivamente.
      analyzePos :: Int -> ExprS
      analyzePos i = analyze (tree `index` i)
      -- | Função auxiliar que retorna o símbolo encontrado na posição `i` da árvore.
      getSymbol :: Int -> String
      getSymbol i = case tree `index` i of
        Leaf symbol -> if (isValidSymbol symbol) then (symbol) else (error "ERRO analyze: símbolo inválido")
        _ -> error "ERRO analyze: símbolo esperado no lugar de uma expressão"

-- | Lista de caracteres e palavras reservadas.
reservedWords :: [String]
reservedWords = ["(", ")", "+", "-", "*", "~", "lambda", "call", "if", "let", "letrec", "cons", "head", "tail", "case", "of"]

-- | Função auxiliar que checa se uma String é uma palavra reservada.
isReservedWord :: String -> Bool
isReservedWord = (`elem` reservedWords)

-- | Função auxiliar que checa se uma String é um símbolo válido.
isValidSymbol :: String -> Bool
isValidSymbol s = not ((isNumeralChar (head s)) || (isReservedWord s))

invalidBool :: String -> Bool
invalidBool [] = True
invalidBool str
  | (str == "true" || str == "false") = False
  | otherwise = True

-- | Função auxiliar de `analyze` para indexação na `ParseTree`.
-- 
-- Realiza `i` movimentos para a direita na árvore e retorna o primeiro
-- elemento à esquerda.
index :: ParseTree -> Int -> ParseTree
index tree i = case tree `skip` i of
  Pair first _ -> first
  _ -> error ("ERRO pos: esperava encontrar elemento da lista na posição " ++ show i)

-- | Função auxiliar de `analyze` para pular elementos da `ParseTree`.
-- 
-- Realiza `i` movimentos para a direita na árvore, retorna erro caso
-- não seja possível.
skip :: ParseTree -> Int -> ParseTree
skip tree i
  | i <= 0 = tree
  | otherwise = next tree i
  where
    next (Pair _ e2) i = skip e2 (i - 1)
    next _ _ = error "ERRO skip: expressão acaba antes do esperado"

-- | Trata o parsing de um token, chamando `parseList` ao encontrar
-- o início de uma lista.
parseToken :: [Token] -> (ParseTree, [Token])
parseToken [] = (Null, [])
parseToken tokens@(token : rest)
  | token == "(" = parseList rest
  | isSymbol token || isNumeral token = (Leaf token, rest)
  | otherwise = (Null, [])

-- | Trata o parsing de uma lista.
parseList :: [Token] -> (ParseTree, [Token])
parseList [] = (Null, [])
parseList tokens@(token : rest)
  | token == ")" = (Null, rest)
  | otherwise = (Pair e1 e2, rest2)
  where
    (e1, rest1) = parseToken tokens
    (e2, rest2) = parseList rest1

-- | Realiza o parsing de múltiplas árvores sintáticas.
parseSequence :: [Token] -> [ParseTree]
parseSequence [] = [Null]
parseSequence tokens =
  exp : parseSequence rest
  where
    (exp, rest) = parseToken tokens

-- | Testa se há um pareamento válido de parêntesis em uma lista de Tokens.
-- 
-- Todo parêntese de abertura "(" deve estar pareado com um parêntese ")",
-- que deve vir após o de abertura. 
--
-- >>> matchingParentheses (tokenize "()(())")
-- True
--
-- >>> matchingParentheses (tokenize "()())(())")
-- False
--
-- >>> matchingParentheses (tokenize "(call (lambda x (+ x 5)) 8)")
-- True
matchingParentheses :: [Token] -> Bool
matchingParentheses = go 0
  where
    go :: Int -> [Token] -> Bool
    go numOpen [] = numOpen == 0
    go numOpen (char : chars) =
      case char of
        "(" -> go (numOpen + 1) chars
        ")" -> (numOpen >= 1) && go (numOpen - 1) chars
        _ -> go numOpen chars
