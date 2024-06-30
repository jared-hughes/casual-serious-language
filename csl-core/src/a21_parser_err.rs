use crate::ast::Ident;
use crate::errors::{def_token_errors, Diag, Diagnostic};
use crate::span::{Span, Spanned};
use crate::token;

const EXAMPLE_FN: &str = "For example: `fn add(x: u64, y: u64) -> u64 { x + y }`";

const EXAMPLE_LET: &str = "For example: `let x = 5;`";

def_token_errors! {
ExpectedConsequent => format!("Unexpected token here. A binary operator like + may be preferred."),
OpenParenMissingCloseParen => format!("Expected to see a ')' here."),
UnaryPlusDisallowed => format!("Leading '+' is not supported."),
UnexpectedBinaryInitial => format!("Unexpected binary operator in initial position."),
UnmatchedCloseParen => format!("What's this ')' doing here? I don't see a '('"),
UnexpectedEOF => format!("Hold your horses. An EOF already?"),
GeneralUnexpected => format!("Unexpected token."),
CallExpComma => format!("Expected ',' after function argument."),
CallExpCloseParen => format!("Expected ')' to end function arguments."),
FnBadComma => format!("Comma ',' is not allowed before first argument."),
ExpectedSemi => format!("Expected semicolon to end the statement"),
FnExpectedName => {
    format!("Expected identifier. Functions must have a name. {EXAMPLE_FN}")
},
ComparatorChainDisallowed => {
    format!("Chaining comparison operations is not yet supported.")
},
}

def_token_errors! {
pub struct FnExpParameter {
    pub fn_name: Ident,
}
msg: self => format!(
    "Expected an identifier to serve as a function parameter for function '{0}'. {EXAMPLE_FN}",
    self.fn_name
);

pub struct FnExpOpenParen {
    pub fn_name: Ident,
}
msg: self => format!(
    "Expected '(' to begin parameter declaration for function '{}'. {EXAMPLE_FN}",
    self.fn_name
);

pub struct FnExpColon {
    pub fn_name: Ident,
    pub param: Ident,
}
msg: self => format!(
    "Expected ':' to provide the type of the parameter '{1}' \
    for function '{0}'. {EXAMPLE_FN}",
    self.fn_name, self.param
);

pub struct FnExpParamType {
    pub fn_name: Ident,
    pub param: Ident,
}
msg: self => format!(
    "Expected an identifier to provide the type of the parameter '{1}' \
    for function '{0}'. {EXAMPLE_FN}",
    self.fn_name, self.param
);

pub struct InvalidToken {
    pub token: token::InvalidToken,
}
msg: self => format!("{}", self.token.msg);

pub struct FnExpComma {
    pub fn_name: Ident,
    pub param_name: Ident,
}
msg: self => format!(
    "Expected ',' after function parameter '{1}' for function '{0}'. {EXAMPLE_FN}",
    self.fn_name, self.param_name
);

pub struct FnExpCloseParen {
    pub fn_name: Ident,
}
msg: self => format!(
    "Expected ')' to end function parameters for function '{0}'. {EXAMPLE_FN}",
    self.fn_name
);

pub struct FnExpThinArrow {
    pub fn_name: Ident,
}
msg: self => format!(
    "Expected '->' to declare return type for function '{0}'. {EXAMPLE_FN}",
    self.fn_name
);

pub struct FnExpReturnType {
    pub fn_name: Ident,
}
msg: self => format!(
    "Expected an identifier to provide the return type for function '{0}'. {EXAMPLE_FN}",
    self.fn_name
);

pub struct FnExpOpenCurly {
    pub fn_name: Ident,
}
msg: self => format!(
    "Expected '{{' to begin the body of function '{0}'. {EXAMPLE_FN}",
    self.fn_name
);

pub struct FnExpCloseCurly {
    pub fn_name: Ident,
}
msg: self => format!(
    "Expected '}}' to end the body of function '{0}'. {EXAMPLE_FN}",
    self.fn_name
);

pub struct LetExpEquals {
    pub ident: Ident,
}
msg: self => format!(
    "Expected an '=', to provide an initial value for '{0}'. {EXAMPLE_LET}",
self.ident);
}
