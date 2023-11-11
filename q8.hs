-- 8) Interseção de duas listas
listInterception [] [] [] = [] -- Nada informado
listInterception [] [] tmp = [] -- Ambas as listas foram processadas
listInterception [] l2 [] = [] 
listInterception [] l2 tmp = [] 
listInterception l1 [] [] = [] 
listInterception l1 [] tmp = listInterception (tail l1) tmp []
listInterception l1 l2 tmp
 | head l1 /= head l2 && null tmp = listInterception l1 (tail l2) l2
 | head l1 /= head l2 && not (null tmp) = listInterception l1 (tail l2) tmp
 | otherwise = head l1 : listInterception (tail l1) (tail l2) tmp


