module Types where

-- | Tokens representam as componentes básicas de um código.
--
-- Exemplos de tokens são números, símbolos, delimitadores de
-- início de fim de listas.
--
-- O tipo `Token` é equivalente ao tipo `String`.
type Token = String

-- | Árvore sintática inicial, sem marcações de semântica
-- 
-- Um valor do tipo `ParseTree` pode ser nulo, uma folha
-- contendo um token, ou uma ramificação de duas árvores
-- diferentes.
-- 
-- O código "(+ 1 2)" pode ser representado como uma ParseTree
-- da seguinte forma:
--
-- >   Pair
-- >  /   \
-- > +   Pair
-- >     /  \
-- >    1   Pair
-- >       /   \
-- >      2    Null
--
-- Note que esta estrutura é muito similar a uma lista ligada.
-- A linguagem interpretada por este programa é um dialeto reduzido
-- de Lisp, cujo nome vem de "List Processing" (ou "processamento de
-- listas"). Em Lisp, listas ligadas são construídas a partir de pares
-- `(x, y)`, nos quais `x` representa um elemento de uma lista e `y`
-- representa o resto da lista. A função que cria tais pares é chamada
-- de `cons`, que vem de "constructor". Uma das características
-- interessantes de diferentes dialetos de Lisp é que os códigos dos
-- programas são definidos por listas ligadas. 
--
-- No entanto, o nome desta estrutura é parse _tree_, e não parse
-- _list_. Se ignorarmos o conteúdo da esquerda de cada par que compõe
-- uma lista, então a `ParseTree` pode ser considerada uma lista.
-- A partir do momento que guardamos _outras_ `ParseTree`s à esquerda
-- de cada par da estrutura, estamos ramificando-a em duas, efetivamente
-- criando uma estrutura de árvore.
--
-- Veja, por exemplo, a representação em árvore do código "(+ (+ 1 2) 3)".
--
-- >     Pair
-- >   /     \
-- >  +      Pair
-- >      /       \
-- >    Pair      Pair
-- >   /   \     /   \
-- >  +   Pair  3    Null
-- >    /    \
-- >   1     Pair
-- >       /    \
-- >      2     Null
--
-- Neste exemplo, a estrutura de árvore de `ParseTree` fica evidente.
--
-- Como curiosidade, as listas ligadas em Haskell podem ser definidas como
--
-- @
-- data List a = Null | Cons a (List a)
-- @
data ParseTree
  = Null
  | Leaf Token
  | Pair ParseTree ParseTree
  deriving (Show, Eq)

-- | Árvore sintática com marcações de semântica, antes do processo
-- de desaçucarização.
--
-- Esta árvore representa uma expressão do código da linguagem intepretada.
-- Todas as construções semânticas (isto é, de significado) da linguagem devem
-- estar expressas em `ExprS`, mesmo que não sejam construções primitivas.
-- Após o processo de desaçucarização, algumas destas construções irão
-- "desaparecer", passando a ser expressas por pelas construções primitivas
-- encontradas em `ExprC`. Por exemplo, a subtração pode ser representada
-- como a adição de um número a outro número multiplicado por -1. Como
-- discutido no arquivo `Desugar.hs`, representar construções como açúcar
-- sintático ou não é algo que deve considerar prós e contras.
--
-- Para adicionar funcionalidade à linguagem, é necessário acrescentar
-- código a esta estrutura.
data ExprS
  = -- | Representa um número.
    NumS     { numS :: Double }
  | -- | Representa um identificador, um nome para valores do ambiente.
    IdS      { idS :: String }
  | -- | Representa a operação de soma.
    PlusS    { leftS :: ExprS, rightS :: ExprS }
  | -- Representa a operação de multiplicação.
    MultS    { leftS :: ExprS, rightS :: ExprS }
  | -- | Representa a operação de subtação.
    BMinusS  { leftS :: ExprS, rightS :: ExprS }
  | -- | Representa o menos unário.
    UMinusS  { bodyS :: ExprS }
  | -- | Representa uma função anônima (isto é, uma função sem nome).
    -- É definida na linguagem a partir do símbolo "lambda".
    LamS     { argNameS :: String, bodyS :: ExprS }
  | -- | Representa a aplicação de uma função.
    AppS     { funS :: ExprS, argS :: ExprS }
  | -- | Representa um desvio com base em uma condição.
    -- Na linguagem, a condição deve ser um número - caso seja igual
    -- a zero, é tratada como falsa, caso contrário, é tratada como
    -- verdadeira.
    IfS      { condS :: String,
               bodyPosS :: ExprS,
               bodyNegS :: ExprS }
  | -- | Representa a criação de um par. Em Lisp, as listas ligadas
    -- são criadas a partir de pares - este vai ser o principal objetivo
    -- de `ConsS` na linguagem definida por este interpretador.
    ConsS    { listHeadS :: ExprS, listTailS :: ExprS }
  | -- | Representa a operação de desconstrução do primeiro elemento de
    -- um ConsS, isto é, representa a cabeça de uma lista.
    HeadS    { listS :: ExprS }
  | -- | Representa a operação de desconstrução do segundo elemento de
    -- um ConsS, isto é, representa a cauda de uma lista.
    TailS    { listS :: ExprS }
  | -- | Representa a criação de uma valor nomeado por um identificador.
    LetS     { nameS :: String, valS :: ExprS, bodyS :: ExprS }
  | -- | Representa a criação de dois valores nomeados por dois identificadores.
    LetStarS { name1S :: String,
               val1S :: ExprS,
               name2S :: String,
               val2S :: ExprS,
               bodyS :: ExprS }
  | -- | Representa a criação de uma função recursiva, que deve ser definida
    -- com uma `lambda`.
    LetrecS  { nameS :: String, valS :: ExprS, bodyS :: ExprS }
  | -- | Transforma qualquer elemento da linguagem em um símbolo.
    QuoteS   { symbolS :: String }
  deriving (Show, Eq)

-- | Árvore sintática com marcações de semântica, depois do processo
-- de desaçucarização.
--
-- Esta árvore representa uma expressão do código da linguagem intepretada,
-- apenas contendo as construções primitivas da linguagem.
--
-- `ExprC` é a estrutura utilizada no processo de avaliação, responsável
-- diretamente por executar o código a ser interpretado. É ideal que
-- tenha a menor quantidade de elementos possíveis, para facilitar o
-- processo de avaliação, possíveis otimizações, e mais.
--
-- Para adicionar funcionalidade à linguagem, é necessário acrescentar
-- código a esta estrutura (a não ser que a funcionalidade possa ser descrita
-- a partir de açúcares sintáticos).
data ExprC
  = -- | Representa um número.
    NumC    { numC :: Double }
  | -- | Representa um identificador, um nome para valores do ambiente. 
    IdC     { idC :: String }
  | -- | Representa a operação de soma.
    PlusC   { leftC :: ExprC, rightC :: ExprC }
  | -- | Representa a operação de multiplicação.
    MultC   { leftC :: ExprC, rightC :: ExprC }
  | -- | Representa uma função anônima (isto é, uma função sem nome).
    -- É definida na linguagem a partir do símbolo "lambda".
    LamC    { argNameC :: String, bodyC :: ExprC }
  | -- | Representa a aplicação de uma função.
    AppC    { funC :: ExprC, argC :: ExprC }
  | -- | Representa um desvio com base em uma condição.
    -- Na linguagem, a condição deve ser um número - caso seja igual
    -- a zero, é tratada como falsa, caso contrário, é tratada como
    -- verdadeira.
    IfC     { condC :: String,
              bodyPosC :: ExprC,
              bodyNegC :: ExprC }
  | -- | Representa a criação de um par. Em Lisp, as listas ligadas
    -- são criadas a partir de pares - este vai ser o principal objetivo
    -- de `ConsC` na linguagem definida por este interpretador.
    ConsC   { listHeadC :: ExprC, listTailC :: ExprC }
  | -- | Representa a operação de desconstrução do primeiro elemento de
    -- um ConsC, isto é, representa a cabeça de uma lista.
    HeadC   { listC :: ExprC }
  | -- | Representa a operação de desconstrução do segundo elemento de
    -- um ConsC, isto é, representa a cauda de uma lista.
    TailC   { listC :: ExprC }
  | -- | Representa a criação de uma função recursiva, que deve ser definida
    -- com uma `lambda`.
    LetrecC { funNameC :: String, argC :: ExprC, bodyC :: ExprC }
  | -- | Transforma qualquer elemento da linguagem em um símbolo.
    QuoteC  { symbolC :: String }
  deriving (Show, Eq)

-- | Valores primitivos da linguagem.
--
-- Representa os valores retornados por expressões avaliadas da linguagem.
data Value
  = -- | Representa um número.
    NumV  { numV :: Double }
  | -- | Representa um fechamento, que é a associação de um código a
    -- ser executado com um ambiente.
    ClosV { argNameV :: String, body :: ExprC, env :: Env }
  | -- | Representa um par, normalmente usado para construir listas ligadas.
    ConsV { headV :: Value, tailV :: Value }
  | -- | Representa um símbolo.
    SymV  { symbolV :: String }
  deriving (Show, Eq)

-- | Vínculo entre um identificador (nome) e um valor.
--
-- É a estrutura guardada pelo ambiente. Cada nome usado em um programa
-- se refere a um valor guardado no ambiente.
-- 
-- Por exemplo, um vínculo `Binding "x" (NumV 1)` representa um vínculo
-- entre um nome "x" e um valor numérico 1. Dado um ambiente com este
-- vínculo armazenado, podemos interpretar algum código que usa "x", como
-- "(+ x 1)", que soma 1 a "x".
data Binding = Binding {nameB :: String, valueB :: Value}
  deriving (Show, Eq)

-- | Ambiente, uma coleção de vínculos nome-valor.
--
-- Este programa implementa o ambiente como uma lista ligada de vínculos.
-- Isto tem algumas implicações na implementação.
--
-- Para ver as funções associadas ao ambiente, veja o arquivo `Env.hs`.
type Env = [Binding]
