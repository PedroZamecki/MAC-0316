
-- Caso tenha o GHC instalado, V. pode executar os testes abaixo usando
-- o comando `runghc Tests.hs` 

module Tests where

import Interpreter (interp0)
import Types

test :: String -> String -> Value -> IO ()
test description code value =
  if interp0 code == value
    then putStrLn $
      "OK " ++ description
    else putStrLn $
      "FAILED " ++ description ++ ". " ++
      "Expected " ++ show value ++ ", " ++
      "got " ++ show (interp0 code)


-- adicione os testes que quiser a mais no main abaixo
main :: IO ()
main = do
  test "Test 1" "(+ 10 (call (lambda x (head x)) (cons 15 16)))" (NumV 25)
  test "Test 2" "(call (lambda x (+ x 5)) 8)" (NumV 13)
  test "Test 3" "(call (lambda f (call f (~ 32))) (lambda x (- 200 x)))" (NumV 232)
  test "Fatorial" "(letrec fatorial (lambda x (if x (* x (call fatorial (- x 1))) 1)) (call fatorial 5))" (NumV 120)

-- ----------------------------------------------------------------------------
-- Caso tenha o Haskell Language Server instalado em sua IDE de escolha,
-- você também pode rodar código da seguinte forma:
-- >>> interp0 "(+ 10 (call (lambda x (head x)) (cons 15 16)))"
-- NumV {numV = 25}

-- >>> interp0 "(call (lambda x (+ x 5)) 8)"
-- NumV {numV = 13}

-- >>> interp0 "(call (lambda f (call f (~ 32))) (lambda x (- 200 x)))"
-- NumV {numV = 232}

-- >>> interp0 "(letrec fatorial (lambda x (if x (* x (call fatorial (- x 1))) 1)) (call fatorial 5))"
-- NumV {numV = 120.0}

