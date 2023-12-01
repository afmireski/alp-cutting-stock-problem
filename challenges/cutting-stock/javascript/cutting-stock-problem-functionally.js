// O problema de corte de estoque envolve otimizar o corte de materiais (geralmente barras
// de comprimento fixo) em pedaços menores para atender a uma série de pedidos com
// comprimentos específicos. Este problema é comumente encontrado em indústrias como
// a de madeira, metal e tecidos. O objetivo é maximizar a utilização do material e
// minimizar o desperdício. Para adaptar esse problema em função de um parâmetro N,
// você pode considerar que existem N pedidos de diferentes comprimentos e barras de
// estoque com um comprimento fixo. O desafio será encontrar a combinação ideal de
// cortes para atender a todos os pedidos, maximizando o número de pedidos atendidos.

const { join } = require("path");

const lerEntrada = (path) => {
  const fs = require("fs");
  try {
    const data = fs.readFileSync(path, { encoding: "utf8" });
    return JSON.parse(data);
  } catch (err) {
    console.log(err);
  }
};

const atendeDemandaRecursiva = (l, demandas) => {

  const atende = (i, cutL, pedidosAtendidos) => {
    if (i >= demandas.length) return pedidosAtendidos;

    let cut = cutL + demandas[i];
    if (cut <= l) {
      demandas[i] == 0;
      pedidosAtendidos++;
      cutL = cut;
      i++;
    } else {
      const delta = l - cutL;
      const list = demandas.filter((e) => e > 0 && e <= delta);
      if (list.length > 0) {
        const max = Math.max(...list);
        cutL += max;

        const j = demandas.findIndex((e) => e === max);
        demandas[j] = demandas[i]; // Envia o elemento que não serviu mais pra frente
        demandas[i] = 0;
        pedidosAtendidos++;
        i++;
      } else {
        cutL = 0;
      }
    }
    if (cutL == l) cutL = 0;
    return atende(i, cutL, pedidosAtendidos);
  };

  return atende(0, 0, 0);
  
  
};

const atendeDemanda = (l, demandas) => {
  let pedidosAtendidos = 0;
  let cutL = 0; 
  let i = 0;
  while (i < demandas.length) {
    let cut = cutL + demandas[i];
    if (cut <= l) {
      demandas[i] == 0;
      pedidosAtendidos++;
      cutL = cut;
      i++;
    } else {
      const delta = l - cutL;
      const list = demandas.filter((e) => e > 0 && e <= delta);
      if (list.length > 0) {
        const max = Math.max(...list);
        cutL += max;

        const j = demandas.findIndex((e) => e === max);
        demandas[j] = demandas[i]; // Envia o elemento que não serviu mais pra frente
        demandas[i] = 0;
        pedidosAtendidos++;
        i++;
      } else {
        cutL = 0;
      }
    }
    if (cutL == l) cutL = 0;
  }
  return pedidosAtendidos;
};

const cutStock = (estoque, demandas) => {
  // Atendendo todos os maiores pedidos primeiros é para haver menos desperdício.
  demandas.sort((a, b) => b - a);

  return atendeDemanda(estoque[0], demandas);
};

const main = () => {
  const { estoque, demandas } = lerEntrada(join(__dirname, "in.json"));

  const pedidos = cutStock(estoque, demandas);
  console.log(`O total de pedidos atendidos foi ${pedidos}`);
};

main();
