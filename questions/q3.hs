-- ListSoma.hs
module ListSoma where

-- Calcular a soma dos elementos de uma lista
listSum :: Num a => [a] -> a
listSum [] = 0
listSum (x : xs) = x + listSum xs