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
        'script': 'cutting-stock-problem-functionally',
        'lang': 'javascript',
        'extension': 'js',
        'runner': 'node'
    },
    {
        'script': 'cutting-stock-problem-imperative',
        'lang': 'javascript',
        'extension': 'js',
        'runner': 'node'
    },
    # {
    #     'script': 'cutting-stock-problem',
    #     'extension': 'hs',
    #     'lang': 'haskell',
    #     'runner': 'runghc'
    # }
]

path = f'{workdir}/challenges/cutting-stock'

input_path = f'{path}/input/in.json'

l = 10
n = 20;
data = {
    'l': l,
    'estoque': generate_fixed_vector(n, l),
    'demandas': generate_random_vector(n, 1, l)  
}



for e in programs:
    input_path = f'{path}/{e["lang"]}/in.json'
    with open(input_path, 'w') as in_json:
        json.dump(data, in_json)
    print(e)
    script = f'{path}/{e["lang"]}/{e["script"]}.{e["extension"]}'
    run_script([e["runner"], script], e["lang"])
