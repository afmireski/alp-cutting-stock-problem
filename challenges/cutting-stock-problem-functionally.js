// O problema de corte de estoque envolve otimizar o corte de materiais (geralmente barras
// de comprimento fixo) em pedaços menores para atender a uma série de pedidos com
// comprimentos específicos. Este problema é comumente encontrado em indústrias como
// a de madeira, metal e tecidos. O objetivo é maximizar a utilização do material e
// minimizar o desperdício. Para adaptar esse problema em função de um parâmetro N,
// você pode considerar que existem N pedidos de diferentes comprimentos e barras de
// estoque com um comprimento fixo. O desafio será encontrar a combinação ideal de
// cortes para atender a todos os pedidos, maximizando o número de pedidos atendidos.

const atendeDemanda = (estoque, nDemanda, lDemanda) => {
  let i = 0;
  let pedidosAtendidos = 0;
  while (nDemanda > 0 && i < estoque.length) {
    if (estoque[i] >= lDemanda) {
      estoque[i] -= lDemanda;
      nDemanda--;
      pedidosAtendidos++;
    } else {
      i++;
    }
  }
  return { estoque, pedidosAtendidos };
};

const cutStock = (n, l, demandas) => {
  let pedidos = 0;
  demandas.sort((a, b) => a.l - b.l);

  let estoque = ;
  for (i = 0; i < n; i++) {
    estoque.push(l);
  }

  demandas.forEach(({ n, l }) => {
    const { estoque: newEstoque, pedidosAtendidos } = (estoque = newEstoque);
    pedidos += pedidosAtendidos;
  });
  
  for (i = 0; i < demandas.length; i++) {
    const { estoque: newEstoque, pedidosAtendidos } = atendeDemanda(
      estoque,
      n,
      l
    );
  }

  return { pedidosAtendidos: pedidos, estoque };
};

function main() {
  const l = 10;
  const n = 10;
  const demandas = [
    {
      n: 5,
      l: 1,
    },
    {
      n: 3,
      l: 8,
    },
    {
      n: 1,
      l: 10,
    },
    {
      n: 3,
      l: 7,
    },
    {
      n: 3,
      l: 2,
    },
    {
      n: 1,
      l: 5,
    },
  ];
  const { pedidosAtendidos: pedidos, estoque } = cutStock(n, l, demandas);
  console.log(
    `O total de pedidos atendidos foi ${pedidos} e o estoque restante é ${estoque}`
  );
}

main();
