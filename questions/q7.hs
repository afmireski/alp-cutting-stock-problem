-- 7) Concatenação de duas listas
concatList [] [] = []
concatList [] l2 = l2
concatList l1 [] = l1
concatList (h1 : t1) l2 = h1 : concatList l2 t1