-- ListSize.hs
module ListSize where

-- Calcular o tamanho de uma lista
listLength :: [a] -> Int
listLength [] = 0
listLength (_ : xs) = 1 + listLength xs