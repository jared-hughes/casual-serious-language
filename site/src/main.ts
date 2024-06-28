import init, { run } from "./csl_web.js";

/** Assume $ always succeeds and returns an HTMLElement */
function $<MatchType extends HTMLElement>(selector: string) {
  return document.querySelector(selector) as MatchType;
}

let wasmReady = false;

const example = `
fn add(x: i64, y: i64) -> i64 {
  ret x + y;
}

fn main() -> i64 {
  ret add(1,2) * 3;
}`.slice(1); // Trim off the leading newline

const form = $<HTMLFormElement>("form");
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
  const outElem = $<HTMLTextAreaElement>("pre#output");
  outElem.innerText = output;
};

function debounce<T extends Function>(cb: T, timeout = 20) {
  let tm = 0;
  const debounced = (...args: any) => {
    clearTimeout(tm);
    tm = setTimeout(() => cb(...args), timeout);
  };
  Object.defineProperty(debounced, "name", { value: `debounced_${cb.name}` });
  return debounced as any as T;
}

function saveToLocalStorage() {
  localStorage.setItem("saved-src", srcInput.value);
}

const srcInput = $<HTMLTextAreaElement>("textarea#src");

const saved = localStorage.getItem("saved-src");
if (saved === null || /^\s+$/.test(saved)) {
  srcInput.value = example;
} else {
  srcInput.value = saved;
}

srcInput.addEventListener("input", debounce(saveToLocalStorage, 1000), false);

init().then(() => {
  wasmReady = true;
});
