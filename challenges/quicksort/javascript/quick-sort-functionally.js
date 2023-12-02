const { join } = require("path");

const lerEntrada = (path) => {
  const fs = require("fs");
  try {   
    const data = fs.readFileSync(path, { encoding: "utf8" });
    return JSON.parse(data)
  } catch (err) {
    console.log(err);
  }
};

function quickSort(arr) {
  if (arr.length <= 1) {
    return arr;
  }

  const pivot = arr[Math.floor(arr.length / 2)];
  const less = arr.filter(element => element < pivot);
  const equal = arr.filter(element => element === pivot);
  const greater = arr.filter(element => element > pivot);

  return [...quickSort(less), ...equal, ...quickSort(greater)];
}

const main = () => quickSort(lerEntrada(join(__dirname, process.argv.splice(2)[0])));

main();