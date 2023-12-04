-- O problema de corte de estoque envolve otimizar o corte de materiais (geralmente barras
-- de comprimento fixo) em pedaços menores para atender a uma série de pedidos com
-- comprimentos específicos. Este problema é comumente encontrado em indústrias como
-- a de madeira, metal e tecidos. O objetivo é maximizar a utilização do material e
-- minimizar o desperdício. Para adaptar esse problema em função de um parâmetro N,
-- você pode considerar que existem N pedidos de diferentes comprimentos e barras de
-- estoque com um comprimento fixo. O desafio será encontrar a combinação ideal de
-- cortes para atender a todos os pedidos, maximizando o número de pedidos atendidos.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Data.List (maximumBy, sortOn)
import Data.Ord (comparing)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory

-- Função para converter uma linha de texto em um vetor de inteiros
linhaParaVetor :: T.Text -> [Int]
linhaParaVetor linha = map (read . T.unpack) $ T.words linha

-- Função para ler um arquivo de texto e converter seu conteúdo para um vetor de inteiros
lerArquivo :: FilePath -> IO [[Int]]
lerArquivo caminhoDoArquivo = do
  conteudo <- TIO.readFile caminhoDoArquivo
  return (map linhaParaVetor (T.lines conteudo))

type Estoque = [Int]

type Demandas = [Int]

-- Função para atender a demanda para um corte específico
atendeDemanda :: Int -> Demandas -> Int
atendeDemanda l demandas =
  atendeDemanda' l 0 demandas 0
  where
    atendeDemanda' :: Int -> Int -> Demandas -> Int -> Int
    atendeDemanda' l cutL [] atendidos = atendidos
    atendeDemanda' l cutL (d : dems) atendidos
      | cut == l = atendeDemanda' l 0 dems (atendidos + 1)
      | cut < l = atendeDemanda' l cut dems (atendidos + 1)
      | otherwise = encontraMelhorCorte l cutL (d : dems) atendidos
      where
        cut = cutL + d

    encontraMelhorCorte :: Int -> Int -> Demandas -> Int -> Int
    encontraMelhorCorte l cutL dems atendidos
      | null list = atendeDemanda' l 0 dems atendidos -- Não achou nada, então zera o cutL e prossegue para o próximo
      | otherwise = atendeDemanda' l (cutL + m) (removeDemanda m dems) (atendidos +1)
      where
        delta = l - cutL
        list = dropWhile (> delta) dems
        m = if null list then 0 else maximum list
    removeDemanda :: Int -> Demandas -> Demandas
    removeDemanda _ [] = []
    removeDemanda d (di : dems)
      | d == di = dems
      | otherwise = di : removeDemanda d dems

-- Função para realizar o corte de estoque
cutStock :: Estoque -> Demandas -> Int
cutStock (l: es) demandas = atendeDemanda l demandas

main :: IO ()
main = do
  diretorioAtual <- getCurrentDirectory
  let caminhoEstoque = diretorioAtual ++ "/haskell/estoque.txt"
  vetorEstoque <- lerArquivo caminhoEstoque
  let estoque = head vetorEstoque

  let caminhoDemandas = diretorioAtual ++ "/haskell/estoque.txt"
  vetorDemandas <- lerArquivo caminhoDemandas

  -- Atendendo todos os maiores pedidos primeiros é para haver menos desperdício.
  let demandas = reverse $ sortOn id (head vetorDemandas) 

  let result = cutStock estoque demandas
  print result
