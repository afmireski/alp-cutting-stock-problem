const { join } = require("path");

function lerEntrada(path) {
  const fs = require("fs");
  try {
    const data = fs.readFileSync(path, { encoding: "utf8" });
    return JSON.parse(data)
  } catch (err) {
    console.error(err);
  }
}

function quickSort(arr, left, right) {
  if (arr.length > 1) {
    left = typeof left != "number" ? 0 : left;
    right = typeof right != "number" ? arr.length - 1 : right;

    let index = partition(arr, left, right);

    if (left < index - 1) {
      quickSort(arr, left, index - 1);
    }

    if (index < right) {
      quickSort(arr, index, right);
    }
  }

  return arr;
}

function partition(arr, left, right) {
  let pivot = arr[Math.floor((left + right) / 2)];
  let i = left;
  let j = right;

  while (i <= j) {
    while (arr[i] < pivot) {
      i++;
    }

    while (arr[j] > pivot) {
      j--;
    }

    if (i <= j) {
      swap(arr, i, j);
      i++;
      j--;
    }
  }

  return i;
}

function swap(arr, leftIndex, rightIndex) {
  let temp = arr[leftIndex];
  arr[leftIndex] = arr[rightIndex];
  arr[rightIndex] = temp;
}

function main() {
  const file = process.argv.splice(2)[0]
  const list = lerEntrada(join(__dirname, file));
  // console.log(list);
  const sortedList = quickSort(list)
  // console.log(sortedList);
}

main();
