{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Data.List (maximumBy, sortOn)
import Data.Ord (comparing)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory
import System.Environment

-- Função para converter uma linha de texto em um vetor de inteiros
linhaParaVetor :: T.Text -> [Int]
linhaParaVetor linha = map (read . T.unpack) $ T.words linha

-- Função para ler um arquivo de texto e converter seu conteúdo para um vetor de inteiros
lerArquivo :: FilePath -> IO [[Int]]
lerArquivo caminhoDoArquivo = do
  conteudo <- TIO.readFile caminhoDoArquivo
  return (map linhaParaVetor (T.lines conteudo))

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

main :: IO()
main = do
     -- Obter os argumentos da linha de comando
    args <- getArgs

    -- Verificar se foram fornecidos argumentos
    case args of
        [] -> putStrLn "Nenhum argumento fornecido."
        _  -> do
            let path = (head args) -- lê o nome do arquivo de entrada
            diretorioAtual <- getCurrentDirectory
            let caminhoDoArquivo = diretorioAtual ++ "/haskell/" ++ path
            vetorDeInteiros <- lerArquivo caminhoDoArquivo
            let myList = head vetorDeInteiros
            let sortedList = quicksort myList
            print "HS Fim"