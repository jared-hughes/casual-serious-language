// src/main.ts
import init, { run } from "./csl_web.js";

// src/examples.ts
var _examples = [
  {
    name: "i64 add",
    source: `fn add(x: i64, y: i64) -> i64 {
      ret x + y;
    }

    fn main() -> i64 {
      ret add(1, 2) * 3;
    }`
  },
  {
    name: "f64 add",
    source: `fn add(x: f64, y: f64) -> f64 {
      ret x + y;
    }

    fn main() -> f64 {
      ret add(1.0, 2.0) * 3.5;
    }`
  },
  {
    name: "let binding",
    source: `fn sum_square(x: f64, y: f64) -> f64 {
      let xx = x * x;
      let yy = y * y;
      ret xx + yy;
    }

    fn main() -> f64 {
      ret sum_square(3.0, 4.0);
    }`
  },
  {
    name: "nested block",
    source: `fn funny_polynomial(x: f64) -> f64 {
      let y = {
        let a = x * x;
        ret a * a + a + 1.0;
      };
      let z = {
        let a = x - 1.0;
        ret a * a - a - 1.0;
      };
      ret y + z;
    }
    fn main() -> f64 {
      ret funny_polynomial(2.0);
    }`
  },
  {
    name: "fibonacci (slow)",
    source: `fn fib(n: i64) -> i64 {
      ret if (n == 0) 0
          else if (n == 1) 1
          else fib(n-1) + fib(n-2);
    }
    fn main() -> i64 {
      ret fib(12);
    }`
  },
  {
    name: "fibonacci (fast)",
    source: `fn f(x: i64, y: i64, n: i64) -> i64 {
      ret if (n == 0) y
          else f(y+x, x, n-1);
    }
    fn fib(n: i64) -> i64 {
      ret f(1,0,n);
    }
    fn main() -> i64 { ret fib(50); }`
  }
];
var examples = _examples.map((ex) => ({
  ...ex,
  source: dedent(ex.source)
}));
function dedent(s) {
  const lines = s.split("\n");
  if (lines.length <= 1) return s;
  let commonWS = leadingWhitespace(lines[1]);
  for (const line of lines.slice(2)) {
    if (/^\s*$/.test(line)) {
      continue;
    }
    const ws = leadingWhitespace(line);
    commonWS = commonPrefix(commonWS, ws);
    if (commonWS.length === 0) return s;
  }
  if (commonWS.length === 0) return s;
  const wsLen = commonWS.length;
  let ret = lines[0];
  for (const line of lines.slice(1)) {
    ret += "\n" + line.slice(wsLen);
  }
  return ret;
}
function leadingWhitespace(line) {
  return line.match(/^\s*/)?.[0] ?? "";
}
function commonPrefix(a, b) {
  const m = Math.min(a.length, b.length);
  for (let i = 0; i < m; i++) {
    if (a[i] !== b[i]) return a.slice(0, i);
  }
  return a.slice(0, m);
}

// src/main.ts
function $(selector) {
  return document.querySelector(selector);
}
var wasmReady = false;
var form = $("form");
form.onsubmit = () => {
  if (!wasmReady) {
    alert("Wasm module not yet loaded");
    return;
  }
  const src = srcInput.value;
  let output;
  try {
    output = run(src);
  } catch (e) {
    output = `Unexpected: ${e}`;
    console.error(e);
  }
  const outElem = $("pre#output");
  outElem.innerText = output;
};
function debounce(cb, timeout = 20) {
  let tm = 0;
  const debounced = (...args) => {
    clearTimeout(tm);
    tm = setTimeout(() => cb(...args), timeout);
  };
  Object.defineProperty(debounced, "name", { value: `debounced_${cb.name}` });
  return debounced;
}
function saveToLocalStorage() {
  localStorage.setItem("saved-src", srcInput.value);
}
var srcInput = $("textarea#src");
var saved = localStorage.getItem("saved-src");
if (saved === null || /^\s+$/.test(saved)) {
  srcInput.value = examples[0].source;
} else {
  srcInput.value = saved;
}
var examplesSpan = $("#examples");
for (const example of examples) {
  const btn = document.createElement("button");
  btn.innerText = example.name;
  btn.addEventListener("click", () => {
    srcInput.value = example.source;
    saveToLocalStorage();
  });
  examplesSpan.appendChild(btn);
  examplesSpan.appendChild(document.createTextNode(" "));
}
srcInput.addEventListener("input", debounce(saveToLocalStorage, 500), false);
init().then(() => {
  wasmReady = true;
});
//# sourceMappingURL=main.js.map
