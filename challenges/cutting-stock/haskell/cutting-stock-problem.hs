-- O problema de corte de estoque envolve otimizar o corte de materiais (geralmente barras
-- de comprimento fixo) em pedaços menores para atender a uma série de pedidos com
-- comprimentos específicos. Este problema é comumente encontrado em indústrias como
-- a de madeira, metal e tecidos. O objetivo é maximizar a utilização do material e
-- minimizar o desperdício. Para adaptar esse problema em função de um parâmetro N,
-- você pode considerar que existem N pedidos de diferentes comprimentos e barras de
-- estoque com um comprimento fixo. O desafio será encontrar a combinação ideal de
-- cortes para atender a todos os pedidos, maximizando o número de pedidos atendidos.

estoque l = [ l | x <- [1..n]] 
resultados = []

atendeDemanda estoque demanda i = sum(estoque) >= demanda !! i

cutStock:: Int -> Int -> [Int]
cutStock n l pedidos =
 | atendeDemanda estoque(l) pedidos=   
 
