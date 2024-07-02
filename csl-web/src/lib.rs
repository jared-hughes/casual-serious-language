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

#[cfg(test)]
mod lib_tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check_run(input: &str, expect: Expect) {
        let out = run(input);
        expect.assert_eq(&out);
    }

    #[test]
    fn smoke() {
        check_run("fn main() -> i64 { ret 2 + 3; }", expect!["5"]);
        check_run(
            "fn main() -> i64 { }",
            expect!["At (!1,1!): Expected function 'main' to return type 'i64', but it returned type '()'"],
        );
    }
}
