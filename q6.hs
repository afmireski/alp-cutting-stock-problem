-- 6) Testar se duas listas são iguais

listIsEqual [] [] = True
listIsEqual list1 list2
 | length list1 /= length list2 = False
 | head list1 /= head list2 = False
 | otherwise = listIsEqual (tail list1) (tail list2)
