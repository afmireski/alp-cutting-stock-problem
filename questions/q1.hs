-- ListMember.hs
module ListMember where

-- Testar se um elemento é membro de uma lista
isElement :: (Eq a) => a -> [a] -> Bool
isElement _ [] = False
isElement x (y : ys) = x == y || isElement x ys