-- 5) Reversão de Lista
concat e list = e : list

reverseList e (h : t)
 | null t  = h : e
 | otherwise = reverseList (h : e) t