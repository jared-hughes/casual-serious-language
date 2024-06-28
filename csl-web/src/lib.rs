use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    pub fn alert(s: &str);
}

#[wasm_bindgen]
pub fn greet(name: &str) -> String {
    alert(&format!("Hello {} from Rust!", name));
    return format!("Hello {} via Rust!", name);
}
