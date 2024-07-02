pub use interpret::compile_and_interpret;

#[path = "a25_ast.rs"]
pub(crate) mod ast;
#[path = "a30_build_mir.rs"]
pub(crate) mod build_mir;
#[path = "a31_build_mir_err.rs"]
pub(crate) mod build_mir_err;
#[path = "b10_errors.rs"]
pub(crate) mod errors;
#[path = "a40_interpret.rs"]
pub(crate) mod interpret;
#[path = "a36_intrinsics.rs"]
pub(crate) mod intrinsics;
#[path = "a10_lexer.rs"]
pub(crate) mod lexer;
#[path = "a35_mir.rs"]
pub(crate) mod mir;
#[path = "a20_parser.rs"]
pub(crate) mod parser;
#[path = "a21_parser_err.rs"]
pub(crate) mod parser_err;
#[path = "b04_pos.rs"]
pub(crate) mod pos;
#[path = "a41_runtime_err.rs"]
pub(crate) mod runtime_err;
#[path = "a45_runtime_value.rs"]
pub(crate) mod runtime_value;
#[path = "b05_span.rs"]
pub(crate) mod span;
#[path = "a29_symbol_table.rs"]
pub(crate) mod symbol_table;
#[cfg(test)]
#[path = "test_helpers.rs"]
pub(crate) mod test_helpers;
#[path = "a15_token.rs"]
pub(crate) mod token;
#[path = "a34_types.rs"]
pub(crate) mod types;
