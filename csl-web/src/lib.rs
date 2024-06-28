use casual_serious_language::compile_and_interpret;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn run(program: &str) -> String {
    let ret = compile_and_interpret(program);
    match ret {
        Ok(val) => format!("{}", val),
        Err(err) => format!("{:#?}", err),
    }
}
