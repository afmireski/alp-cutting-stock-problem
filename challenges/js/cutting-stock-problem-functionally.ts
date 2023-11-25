// O problema de corte de estoque envolve otimizar o corte de materiais (geralmente barras
// de comprimento fixo) em pedaços menores para atender a uma série de pedidos com
// comprimentos específicos. Este problema é comumente encontrado em indústrias como
// a de madeira, metal e tecidos. O objetivo é maximizar a utilização do material e
// minimizar o desperdício. Para adaptar esse problema em função de um parâmetro N,
// você pode considerar que existem N pedidos de diferentes comprimentos e barras de
// estoque com um comprimento fixo. O desafio será encontrar a combinação ideal de
// cortes para atender a todos os pedidos, maximizando o número de pedidos atendidos.

// const atendeDemanda = (estoque, nDemanda, lDemanda) => {
//   let i = 0;
//   let pedidosAtendidos = 0;
//   while (nDemanda > 0 && i < estoque.length) {
//     if (estoque[i] >= lDemanda) {
//       estoque[i] -= lDemanda;
//       nDemanda--;
//       pedidosAtendidos++;
//     } else {
//       i++;
//     }
//   }
//   return { estoque, pedidosAtendidos };
// };

// const cutStock = (n, l, demandas) => {
//   let pedidos = 0;
//   demandas.sort((a, b) => a.l - b.l);

//   let estoque = ;
//   for (i = 0; i < n; i++) {
//     estoque.push(l);
//   }

//   demandas.forEach(({ n, l }) => {
//     const { estoque: newEstoque, pedidosAtendidos } = (estoque = newEstoque);
//     pedidos += pedidosAtendidos;
//   });

//   for (i = 0; i < demandas.length; i++) {
//     const { estoque: newEstoque, pedidosAtendidos } = atendeDemanda(
//       estoque,
//       n,
//       l
//     );
//   }

//   return { pedidosAtendidos: pedidos, estoque };
// };

type Barra = { n: number; l: number };
type AtendeDemandaResponse = { estoque: number[]; pedidosAtendidos: number };

const atendeDemanda = (
  l: number,
  estoque: number[],
  demandas: Barra[]
): AtendeDemandaResponse => {
  let i = 0;
  let j = 0;
  let pedidosAtendidos = 0;
  let cutL = 0;

  const nextEstoque = (cut?: () => void) => {
    if (cut) cut();
    i++;
    cutL = 0;
  };

  const atendeuDemanda = (k: number, op: () => void) => {
    op();
    demandas[k].n--;
    pedidosAtendidos++;
  };

  // Busca pela primeira demanda que gere um corte melhor do que o atual
  const buscaCorte = (lDemanda: number) => {
    let betterCut = lDemanda + cutL;
    let betterK = j;

    // Busca por alguma outra demanda que não extrapole o tamanho do corte
    for (let k = j; k < demandas.length; k++) {
      let cut = demandas[k].l + cutL;
      if (cut > betterCut && cut <= l) {
        betterCut = cut;
        betterK = k;
        if (cut == l) break; // A demanda liquida aquele item do estoque
      }
    }

    if (betterCut <= l) {
      // Encontrou alguma demanda que proporciona um aproveitamento melhor
      atendeuDemanda(betterK, () => (estoque[i] -= betterCut));

      if (estoque[i] == 0) {
        nextEstoque();
      }
    } else {
      // Não encontrou nenhuma demanda que proporcione um corte melhor
      nextEstoque(() => (estoque[i] -= cutL));
    }
  };

  while (cutL < l && i < estoque.length && j < demandas.length) {
    let { n: nDemanda, l: lDemanda } = demandas[j];

    if (nDemanda > 0) {
      if (lDemanda + cutL <= l) {
        atendeuDemanda(j, () => (cutL += lDemanda));

        if (cutL == l) {
          // Acabou com o estoque
          nextEstoque(() => (estoque[i] -= cutL));
        }
      } else {
        buscaCorte(lDemanda);
      }

      if (demandas[j].n == 0) j++;
    }
  }

  return { estoque, pedidosAtendidos };
};

const cutStock = (n: number, l: number, demandas: Barra[]) => {
  let pedidos = 0;
  demandas.sort((a, b) => a.l + b.l);

  let estoque: number[] = [];
  for (let i = 0; i < n; i++) estoque.push(l);

  const { estoque: newEstoque, pedidosAtendidos } = atendeDemanda(
    l,
    estoque,
    demandas
  );

  return { pedidosAtendidos, estoque: newEstoque };
};

const main = () => {
  const l = 10;
  const n = 10;
  const demandas: Barra[] = [
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
};

main();
