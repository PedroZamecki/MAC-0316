module Desugar where

import Types (ExprS (..), ExprC (..))

-- | _Desaçucarizador_.
--
-- O processo de desaçucarização nos permite representar açúcares
-- sintáticos em termos das construções principais da linguagem.
--
-- Açúcares sintáticos são extensões a uma linguagem que facilitam
-- seu uso a um programador, mas que podem ser definidas em termos de
-- construções já existentes da linguagem. São formas de deixar uma
-- linguagem mais fácil de se utilizar e ao mesmo tempo evitar
-- complexidade extra a um compilador ou interpretador.
--
-- Por exemplo, podemos representar o menos unário (dado nesta
-- linguagem pelo caracter '~') como uma multiplicação por -1, ou como
-- uma subtração de 0 pelo número em questão. Pode ser que esta
-- mudança de representação resulte em performance melhor ou não;
-- é desejável fazer testes e/ou trazer argumentos para determinar
-- se uma desaçucarização vale a pena.
--
-- >>> desugar (UMinusS (NumS 10))
-- MultC {leftC = NumC {numC = -1}, rightC = NumC {numC = 10}}
--
-- Para adicionar funcionalidade à linguagem, é necessário acrescentar
-- código a esta função.
desugar :: ExprS -> ExprC
desugar expr = case expr of
  NumS  num      -> NumC num
  IdS   sym      -> IdC sym
  PlusS e1 e2    -> PlusC (desugar e1) (desugar e2)
  MultS e1 e2    -> MultC (desugar e1) (desugar e2)
  BMinusS e1 e2  -> PlusC (desugar e1) (MultC (NumC (-1)) (desugar e2))
  UMinusS e1     -> MultC (NumC (-1)) (desugar e1)
  LamS argName b -> LamC argName (desugar b)
  AppS fun arg   -> AppC (desugar fun) (desugar arg)
  IfS cond b1 b2 -> IfC cond (desugar b1) (desugar b2)
  ConsS e1 e2    -> ConsC (desugar e1) (desugar e2)
  HeadS e1       -> HeadC (desugar e1)
  TailS e1       -> TailC (desugar e1)
  LetS name val body ->
    AppC
      (LamC name (desugar body))
      (desugar val)
  LetStarS name1 val1 name2 val2 body ->
    AppC
      (LamC name1 
        (AppC (LamC name2 (desugar body))
        (desugar val2)))
      (desugar val2)
  LetrecS name val body -> LetrecC name (desugar val) (desugar body)
  QuoteS symbol -> QuoteC symbol
