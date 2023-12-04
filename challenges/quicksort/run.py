import subprocess
import time
import os
import random
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns


def run_script(cmd, name):
    # try:
        start_time = time.time()

        subprocess.run(cmd)

        end_time = time.time()
        # print(end_time)
        delta = end_time - start_time
        return delta
    # except subprocess.CalledProcessError as e:
    #     return delta # Tempo de execução

def generate_random_vector(length):
    return [random.randint(1, 10000) for _ in range(length)]

def writeInputFiles(n):    
    os.system(f'./generate-rand-vector {n} in{n}.txt')

workdir = os.getcwd()
programs = [
    {
        'script': 'quick-sort',
        'extension': 'hs',
        'lang': 'haskell',
        'runner': 'runghc',
        'label': 'Haskell',
    },
    {
        'script': 'quick-sort-imperative',
        'lang': 'javascript',
        'extension': 'js',
        'runner': 'node',
        'label': 'JS-Imp',
        'input_ext': '.json'
    },
    {
        'script': 'quick-sort-functionally',
        'lang': 'javascript',
        'extension': 'js',
        'runner': 'node',
        'label': 'JS-Fun',
        'input_ext': '.json'
    },
]

path = f'{workdir}/challenges/quicksort'


# O tempos dependem do vetor aleatório gerado
elements = [
    19000000, # ~1s haskell
    # 70000000, # ~10s JS Imperative
    # 300000000,
    400000000, # ~10s Haskell
    600000000 # Limite Haskell ~40 s
]

# Gera o executável do gerador de vetor
subprocess.run(['clang', 'generate-rand-vector.c', '-o', 'generate-rand-vector']) 

# Descomente se precisar gerar as entradas
# for n in elements:
#     start_time = time.time()
#     data = generate_random_vector(n)    
#     writeInputFiles(n)      
#     end_time = time.time()
#     delta = end_time - start_time

size = len(elements) * len(programs)
datasets = []
index_list = [1] # lista de index pro seaborn parar de reclamar
i = 1

for n in elements:
    for code in programs:
        index_list[0] = i
        script = f'./{code["lang"]}/{code["script"]}.{code["extension"]}'
        execution_time = run_script([code["runner"], script, f'in{n}.txt'], code["lang"])
        print(f't = {execution_time} s')

        if execution_time != -1:
            e = dict(language=code['label'],n=n,time=execution_time)
            df = pd.DataFrame(e, index=index_list, columns=['language', 'n', 'time'])
            datasets.append(df)
            i+=1

# cria axis para controlar melhor elementos do grafico
dataset = pd.concat(datasets)  # Concatena os datasets
fig, ax = plt.subplots()

# # remove as bordas para melhor visualizacao
sns.despine(right=True, top=True, ax=ax)
sns.set_style('ticks')
sns.lineplot(x='n', y='time', hue='language', data=dataset, ax=ax, markers=True, style='language', legend='brief')

# # Define o título e os labels dos eixos
ax.set_title('Tempo de execução para ')
ax.set_xlabel('Nº de Elementos')
ax.set_ylabel('Tempo (s)')
ax.legend(ncol=1, frameon=False, fontsize=12)

plt.show()
fig.tight_layout()
fig.savefig('./output/resultados.pdf') # Salva o gráfico como PDF
dataset.to_csv('./output/dataset.csv')
