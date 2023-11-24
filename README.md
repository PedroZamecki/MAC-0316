# Interpretador de MAC0316

Este projeto contêm o código para o interpretador de uma linguagem baseada em Lisp.

Para entender como o interpretador funciona, comece pela documentação disponível nos arquivos `Main.hs` e `Interpreter.hs`.

## Sintaxe da linguagem do interpretador

Uma _gramática_ no formato Backus-Naur (BNF) para a linguagem de programação do interpretador está definida no enunciado do EP.

Uma gramática define as regras de substituição para formar elementos de uma linguagem. De forma resumida, cada variável à esquerda de uma regra (as regras são definidas pelo operador `::=`) representa algo que pode ser substituído por um elemento da direita. O operador `|` é parecido com um _ou lógico_ - uma variável `<x>` na regra `<x> ::= <a> | <b>` pode ser substituída por `<a>` ou por `<b>`. Texto entre aspas representa texto que deve permanecer da forma que está após feita a substituição. Ao final das substituições, todas as variáveis devem ter se transformado em símbolos terminais (texto propriamente dito).



## Instalação do Haskell

O interpretador é escrito na linguagem de programação Haskell. Para executá-lo, é necessário instalar o ferramental da linguagem.

O processo de instação pode ser feito de diversas maneiras, a depender do sistema operacional. Uma ferramenta de instalação comumente usada é o **GHCup**, disponível em <https://www.haskell.org/ghcup/>.

## Execução do projeto

Uma vez que o ferramental do Haskell esteja instalado, podemos executar o projeto. Haskell é uma linguagem cujo código pode ser executado tanto de forma interpretada quanto compilada.

Para executar o REPL do interpretador (veja `Main.hs` para uma explicação) de forma interpretada, você pode executar o comando

```
runghc Main.hs
```

Caso deseje compilar o projeto, você pode usar o comando

```
ghc -O2 Main.hs -o interpretador
```

e executar o programa com

```
./interpretador
```

Este programa propositalmente não usa ferramentas de gerenciamento de projeto como o [Stack](https://www.haskellstack.org/) ou o [Cabal](https://www.haskell.org/cabal/) para reduzir atrito a usuários novos à linguagem.

## Testes

Os testes do projeto podem ser executados com o comando

```
runghc Tests.hs
```

## Brincando com o interpretador

O ferramental de Haskell vem com um programa chamado **GHCi**, um REPL para a linguagem. Ele pode ser usado para interagir com o código de um projeto de forma fácil e rápida.

Para abrir a interface do GHCi, use o comando

```
ghci
```

Arquivos do projeto podem ser carregados usando o comando `:load` do GHCi. Por exemplo, podemos carregar o arquivo `Main.hs` usando

```
ghci> :load Main.hs
```

Tendo o código carregado no GHCi, podemos testar cada função do projeto individualmente. Por exemplo, podemos verificar o resultado da função `parse` para um código qualquer

```
ghci> import Tokenizer
ghci> import Parser
ghci> parse (tokenize "(+ 1 2)")
Pair (Leaf "+") (Pair (Leaf "1") (Pair (Leaf "2") Null))
```

## Considerações finais

O código deste projeto foi escrito pelo monitor de MAC0316 Eduardo Sandalo Porto no oferecimento de 2022, com base no material do livro _Programming Languages: Application and Interpretation_, por _Shriram Krishnamurthi_. Quaisquer problemas com o código ou dificuldades com o exercício podem ser passados tanto à professora quanto ao monitor. Nossos canais de contato podem ser encontrados na página da disciplina no Moodle.
