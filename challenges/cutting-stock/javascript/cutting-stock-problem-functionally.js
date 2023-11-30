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
}


const atendeDemanda = (l, estoque, demandas) => {
  let i = 0;
  let j = 0;
  let pedidosAtendidos = 0;
  let cutL = 0;

  const nextEstoque = (cut) => {
    if (cut) cut();
    i++;
    cutL = 0;
  };

  const atendeuDemanda = (k, op) => {
    op();
    demandas[k] = 0;
    pedidosAtendidos++;
  };

  // Busca pela primeira demanda que gere um corte melhor do que o atual
  const buscaCorte = (lDemanda) => {
    let betterCut = lDemanda + cutL;
    let betterK = j;

    // Busca por alguma outra demanda que não extrapole o tamanho do corte
    for (let k = j; k < demandas.length; k++) {
      let cut = demandas[k] + cutL;
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
    let lDemanda = demandas[j];

    if (lDemanda > 0) {
      if (lDemanda + cutL <= l) {
        atendeuDemanda(j, () => (cutL += lDemanda));

        if (cutL == l) {
          // Acabou com o estoque
          nextEstoque(() => (estoque[i] -= cutL));
        }
      } else {
        buscaCorte(lDemanda);
      }

      if (demandas[j] == 0) j++;
    }
  }

  return { estoque, pedidosAtendidos };
};

const cutStock = (estoque, demandas) => {
  const { estoque: newEstoque, pedidosAtendidos } = atendeDemanda(
    estoque[0],
    estoque,
    demandas
  );

  return { pedidosAtendidos, estoque: newEstoque };
};

const main = () => {
  const { estoque, demandas } = lerEntrada(
    join(__dirname, 'in.json')
  );
  // console.log("Estoque -> ", estoque);
  // console.log("Demandas -> ", demandas);

  const { pedidosAtendidos: pedidos, estoque: estoqueFinal } = cutStock(estoque, demandas);
  console.log(
    `O total de pedidos atendidos foi ${pedidos}`
  );
};

main();
