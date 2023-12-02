import subprocess
import time
import os
import random
import json
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns


def run_script(cmd, name):
    start_time = time.time()
    print(start_time)

    subprocess.run(cmd)

    end_time = time.time()
    print(end_time)
    delta = end_time - start_time

    return delta # Tempo de execução

def generate_random_vector(length, min, max):
    return [random.randint(min, max) for _ in range(length)]

def generate_fixed_vector(length, value):
    return [value for _ in range(length)]

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
    # {
    #     'script': 'cutting-stock-problem-functionally',
    #     'lang': 'javascript',
    #     'extension': 'js',
    #     'runner': 'node',
    #     'label': 'JS-Fun',
    #     'input_ext': '.json'
    # },
    # {
    #     'script': 'cutting-stock-problem-imperative',
    #     'lang': 'javascript',
    #     'extension': 'js',
    #     'runner': 'node',
    #     'label': 'JS-Imp',
    #     'input_ext': '.json'
    # }
]

path = f'{workdir}/challenges/cutting-stock'


l = 100
elements = [
    190000, # ~1s
    800000, # ~10s
    # 1000 # ~1 min
]

size = len(elements) * len(programs)
datasets = []
index_list = [x+1 for x in range(size)]
i = 0;
for n in elements:
    data = {
        'l': l,
        'estoque': generate_fixed_vector(n, l),
        'demandas': generate_random_vector(n, 1, l)  
    }
    for code in programs:
        input_path = f'{path}/{code["lang"]}/in{code["input_ext"]}'
        with open(input_path, 'w') as in_file:
            if (code['input_ext'] == '.txt'):
                estoque = ''
                for e in data['estoque']:
                    estoque+=f'{e} '
                estoque.strip()
                estoque+='\n'

                demandas = ''
                for d in data['demandas']:
                    demandas+=f'{d} '
                demandas.strip()
                demandas+='\n'
                in_file.writelines([estoque, demandas])
                
            else:
                json.dump(data, in_file)
        script = f'{path}/{code["lang"]}/{code["script"]}.{code["extension"]}'
        execution_time = run_script([code["runner"], script], code["lang"])
        print(f't = {execution_time} s')
        # e = dict(language=code['label'],n=n,time=execution_time)
        # df = pd.DataFrame(e, index=index_list, columns=['language', 'n', 'time'])
        # datasets.append(df)
        # i+=1

# cria axis para controlar melhor elementos do grafico

# dataset = pd.concat(datasets)  # Concatena os datasets
# fig, ax = plt.subplots()

# # remove as bordas para melhor visualizacao
# sns.despine(right=True, top=True, ax=ax)
# sns.set_style('ticks')
# sns.lineplot(x='time', y='n', hue='language', data=dataset, markers=True, ax=ax, legend='brief')

# # Define o título e os labels dos eixos
# ax.set_title('Tempo de execução para ')
# ax.set_xlabel('Nº de Elementos')
# ax.set_ylabel('Tempo (s)')
# ax.legend(ncol=1, frameon=False, fontsize=12)

# plt.show()
# fig.tight_layout()
# fig.savefig('./output/resultados.pdf') # Salva o gráfico como PDF
# dataset.to_csv('./output/dataset.csv')
