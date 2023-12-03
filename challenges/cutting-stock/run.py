import subprocess
import time
import os
import random
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns


def run_script(cmd, name):
    start_time = time.time()
    # print(start_time)

    subprocess.run(cmd)

    end_time = time.time()
    # print(end_time)
    delta = end_time - start_time

    return delta # Tempo de execução

def generate_random_vector(length, min, max):
    return [random.randint(min, max) for _ in range(length)]

def generate_fixed_vector(length, value):
    return [value for _ in range(length)]

def writeInputFiles(n, l):
    data = {
        'l': l,
        'estoque': generate_fixed_vector(n, l),
        'demandas': generate_random_vector(n, 1, l)  
    }

    path1 = f'./haskell/estoque.txt'
    path2 = f'./javascript/estoque.txt'
    estoque1 = ''
    estoque2 = '['
    for e in data['estoque']:
        estoque1+=f'{e} '
        estoque2+=f'{e},'
    string_list = list(estoque2)
    string_list[len(string_list)-1] = ']'
    estoque2=''.join(string_list)
    estoque1.strip()


    path3 = f'./haskell/demandas.txt'
    path4 = f'./javascript/demandas.txt'
    demandas1 = ''
    demandas2 = '['
    for d in data['demandas']:
        demandas1+=f'{d} '
        demandas2+=f'{d},'
    demandas1.strip()
    string_list = list(demandas2)
    string_list[len(string_list)-1] = ']'
    demandas2=''.join(string_list)

    with open(path1, 'w') as file:
        file.writelines(estoque1)
    
    with open(path2, 'w') as file:
        file.writelines(estoque2)            
    
    with open(path3, 'w') as file:
        file.writelines(demandas1)
    
    with open(path4, 'w') as file:
        file.writelines(demandas2)
        

workdir = os.getcwd()
programs = [
    {
        'script': 'cutting-stock-problem',
        'extension': 'hs',
        'lang': 'haskell',
        'runner': 'runghc',
        'label': 'Haskell',
        'input_ext': '.txt'
    },
    {
        'script': 'cutting-stock-problem-imperative',
        'lang': 'javascript',
        'extension': 'js',
        'runner': 'node',
        'label': 'JS-Imp',
        'input_ext': '.json'
    },
    {
        'script': 'cutting-stock-problem-functionally',
        'lang': 'javascript',
        'extension': 'js',
        'runner': 'node',
        'label': 'JS-Fun',
        'input_ext': '.json'
    }
]



l = 1000
elements = [
    450000, # ~1s
    5000000, # ~10s
    15000000 # ~1 min
]

size = len(elements) * len(programs)
datasets = []

index_list = [1] # lista de index pro seaborn parar de reclamar
i = 1
for n in elements:
    writeInputFiles(n=n, l=l)
    for code in programs:    
        index_list[0] = i    
        script = f'./{code["lang"]}/{code["script"]}.{code["extension"]}'
        execution_time = run_script([code["runner"], script], code["lang"])
        print(f't = {execution_time} s')
        e = dict(language=code['label'],n=n,time=execution_time)
        df = pd.DataFrame(e, index=index_list, columns=['language', 'n', 'time'])
        datasets.append(df)
        i+=1

# cria axis para controlar melhor elementos do grafico

dataset = pd.concat(datasets)  # Concatena os datasets
fig, ax = plt.subplots()

# remove as bordas para melhor visualizacao
sns.despine(right=True, top=True, ax=ax)
sns.set_style('ticks')
sns.lineplot(x='n', y='time', hue='language', data=dataset, ax=ax, markers=True, style='language', legend='brief')

# Define o título e os labels dos eixos
ax.set_title('Tempo de execução para ')
ax.set_xlabel('Nº de Elementos')
ax.set_ylabel('Tempo (s)')
ax.legend(ncol=1, frameon=False, fontsize=12)

plt.show()
fig.tight_layout()
fig.savefig('./output/resultados.pdf') # Salva o gráfico como PDF
dataset.to_csv('./output/dataset.csv')
