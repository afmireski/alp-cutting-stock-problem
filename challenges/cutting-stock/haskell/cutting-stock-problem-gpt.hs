-- O problema de corte de estoque envolve otimizar o corte de materiais (geralmente barras
-- de comprimento fixo) em pedaços menores para atender a uma série de pedidos com
-- comprimentos específicos. Este problema é comumente encontrado em indústrias como
-- a de madeira, metal e tecidos. O objetivo é maximizar a utilização do material e
-- minimizar o desperdício. Para adaptar esse problema em função de um parâmetro N,
-- você pode considerar que existem N pedidos de diferentes comprimentos e barras de
-- estoque com um comprimento fixo. O desafio será encontrar a combinação ideal de
-- cortes para atender a todos os pedidos, maximizando o número de pedidos atendidos.
{-# LANGUAGE OverloadedStrings #-}

import Data.List (sortBy, maximumBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
atendeDemanda :: Int -> Estoque -> Demandas -> (Estoque, Int)
atendeDemanda l estoque demandas =
  atendeDemanda' 0 0 0 l estoque demandas
  where
    atendeDemanda' :: Int -> Int -> Int -> Int -> Estoque -> Demandas -> (Estoque, Int)
    atendeDemanda' i j cutL l' est dems
      | cutL >= l' || i >= length est || j >= length dems = (est, 0)
      | lDemanda > 0 = if lDemanda + cutL <= l'
        then if cutL == l'
          then let (newEst, atendidos) = atendeDemanda' (i + 1) j 0 l' est dems
               in (newEst, atendidos + 1)
          else let newDems = atualizarDemandas j dems
               in atendeDemanda' i (j + 1) (cutL + lDemanda) l' est newDems
        else let melhorCorte = encontraMelhorCorte cutL l' j dems
             in if melhorCorte <= l'
                  then let newEst = atualizarEstoque i cutL est
                           newDems = atualizarDemandas melhorCorte dems
                           (nextEst, atendidos) = if newEst !! i == 0
                                                    then atendeDemanda' (i + 1) j 0 l' newEst newDems
                                                    else atendeDemanda' i j 0 l' newEst newDems
                       in (nextEst, atendidos + 1)
                  else let (newEst, atendidos) = atendeDemanda' (i + 1) j 0 l' est dems
                       in (newEst, atendidos)
      | otherwise = atendeDemanda' i (j + 1) cutL l' est dems
      where
        lDemanda = dems !! j

    encontraMelhorCorte :: Int -> Int -> Int -> Demandas -> Int
    encontraMelhorCorte cutL l' j dems =
      fst $ maximumBy (comparing snd) corteDemandaTuples
      where
        corteDemandaTuples = [(cut, cut + lDemanda) | (cut, lDemanda) <- cortes]
        cortes = [(cutL + lDemanda, lDemanda) | (lDemanda, k) <- zip (drop j dems) [j..], cutL + lDemanda <= l']

    atualizarEstoque :: Int -> Int -> Estoque -> Estoque
    atualizarEstoque i cutL est =
      take i est ++ [est !! i - cutL] ++ drop (i + 1) est

    atualizarDemandas :: Int -> Demandas -> Demandas
    atualizarDemandas cutL dems =
      takeWhile (> 0) (takeWhile (<= cutL) dems ++ dropWhile (<= cutL) dems)

-- Função para realizar o corte de estoque
cutStock :: Estoque -> Demandas -> (Estoque, Int)
cutStock estoque demandas =
  atendeDemanda (head estoque) estoque demandas



main :: IO ()
main = do
    diretorioAtual <- getCurrentDirectory
    let caminhoDoArquivo = diretorioAtual ++ "/challenges/cutting-stock/haskell/in.txt"
    vetorDeInteiros <- lerArquivo caminhoDoArquivo
    let estoque = head vetorDeInteiros
    let demandas = head (tail vetorDeInteiros)

    let result = cutStock estoque demandas
    putStrLn $ "O resultado é:" ++ show result 
