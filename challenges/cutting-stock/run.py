import subprocess
import time
import os
import random
import json


def run_script(cmd, name):
    start_time = time.time()

    subprocess.run(cmd)

    end_time = time.time()
    delta = end_time - start_time

    print(f'{name} demorou {delta:.2f} segundos\n')

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
        'input_ext': '.txt'
    },
    {
        'script': 'cutting-stock-problem-functionally',
        'lang': 'javascript',
        'extension': 'js',
        'runner': 'node',
        'input_ext': '.json'
    },
    {
        'script': 'cutting-stock-problem-imperative',
        'lang': 'javascript',
        'extension': 'js',
        'runner': 'node',
        'input_ext': '.json'
    }
]

path = f'{workdir}/challenges/cutting-stock'


l = 100
n = 200000;
data = {
    'l': l,
    'estoque': generate_fixed_vector(n, l),
    'demandas': generate_random_vector(n, 1, l)  
}



for e in programs:
    input_path = f'{path}/{e["lang"]}/in{e["input_ext"]}'
    with open(input_path, 'w') as in_file:
        if (e['input_ext'] == '.txt'):
            estoque = ''
            for n in data['estoque']:
                estoque+=f'{n} '
            estoque.strip()
            estoque+='\n'

            demandas = ''
            for n in data['demandas']:
                demandas+=f'{n} '
            demandas.strip()
            demandas+='\n'
            in_file.writelines([estoque, demandas])
            
        else:
            json.dump(data, in_file)
    print(e)
    script = f'{path}/{e["lang"]}/{e["script"]}.{e["extension"]}'
    run_script([e["runner"], script], e["lang"])
