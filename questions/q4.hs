-- ListProd
module ListProd where

-- Calcular o produto dos elementos de uma lista
listProduct :: Num a => [a] -> a
listProduct [] = 1
listProduct (x : xs) = x * listProduct xs
