# alp-cutting-stock-problem
Lista de exercícios com problemas a serem resolvidos utilizando a linguagem funcional Haskell, incluindo o problema de corte de estoque.

## Questões simples
1) Testar se um elemento é membro de uma lista
2) Calcular o tamanho de uma lista
3) Calcular a soma dos elementos de uma lista
4) Calcular o produto dos elementos de uma lista
5) Reversão de lista
6) Testar se duas listas são iguais
7) Concatenação de duas listas
8) Interseção de duas listas

## Desafios
1) Quicksort
2) Problema do Corte de Estoque

_Esses desafios terão três resoluções distintas:_
- Em Haskell
- Em Javascript sem utilizar construções funcionais.
- Em Javascript com utilização de construções funcionais.

## Execução
As questões simples podem ser executadas através do `ghci`:
```zsh
cd ./questions # Seleciona a pasta das questões simples

ghci q<n>.hs # Escolher arquivo que será carregado
```
Já os desafios são mais elaborados, como eles exigem a montagem de um gráfico com os resultados das três execuções, foi criado um script em python que gera as entradas necessárias e testa as três abordagens para cada um dos três tempos de execução:
1. ~1s
2. ~10s
3. ~1 min  
Esses casos são baseados no tempo de execução da Haskell.
Após cada execução o tempo decorrido é calculado e utilizado para montar um gráfico para as três linguagens.

### Instalando dependências
Os gráficos são montados utilizando três bibliotecas do python: `matplotlib`, `pandas` e `seaborn`, caso precise instalá-las:
```zsh
# Arch
sudo pacman -S python-pandas

sudo pacman -S python-matplotlib

sudo pacman -S python-seaborn

# PyPi
pip install pandas

pip install matplotlib

pip install seaborn
```
- Caso precise instalar a Haskell, veja as instruções [aqui](https://www.haskell.org/ghcup/).
- Caso precise instalar o node, veja as instruções [aqui](https://nodejs.org/en/download/package-manager)

### Quicksort
```zsh
# Acesse a pasta do desafio
cd ./challenges/quicksort

# Caso precise gerar as entradas descomente esse trecho no script:
for n in elements:
    start_time = time.time()
    data = generate_random_vector(n)    
    writeInputFiles(n)      
    end_time = time.time()
    delta = end_time - start_time 

# ou execute generate-rand-vector passando seus parâmetros:
./generate-rand-vector n in{n}.txt

# Execute o script em python
python3 run.py

# Ao final compare seus resultados em ./output com os arquivos -oficial
```

### Cutting-Stock-problem
**Aqui os códigos JS tem uma trava de execução que os encerra após 4 minutos, pois percebemos que eles ficaram bem ineficientes para valores muito grandes**
```zsh
# Acesse a pasta do desafio
cd ./challenges/cutting-stock

# Execute o script em python
python3 run.py

# Ao final compare seus resultados em ./output com os arquivos -oficial
```

