import init, { greet } from "./csl_web.js";

/** Assume $ always succeeds and returns an HTMLElement */
function $<MatchType extends HTMLElement>(selector: string) {
  return document.querySelector(selector) as MatchType;
}

let wasmReady = false;

const form = $<HTMLFormElement>("form#name-form");
form.onsubmit = () => {
  const name = $<HTMLInputElement>("input#name").value;
  if (!wasmReady) {
    alert("Wasm module not yet loaded");
    return;
  }
  let s = greet(name);
  alert(s + " -- from JS!");
};

init().then(() => {
  wasmReady = true;
});
