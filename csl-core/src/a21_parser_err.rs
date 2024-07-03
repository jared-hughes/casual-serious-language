use crate::ast::Ident;
use crate::errors::{def_token_errors, Diag, Diagnostic};
use crate::span::{Span, Spanned};
use crate::token;

const EXAMPLE_FN: &str = "For example: `fn add(x: u64, y: u64) -> u64 { x + y }`";
const EXAMPLE_LET: &str = "For example: `let x = 5;`";
const EXAMPLE_ASSIGN: &str = "For example: `x = 5;`";
const EXAMPLE_IF: &str = "For example: `if (x > 5) 1 else 0`.";

def_token_errors! {
ExpectedConsequent => format!("Unexpected token here. A binary operator like + may be preferred."),
OpenParenMissingCloseParen => format!("Expected to see a ')' here."),
UnaryPlusDisallowed => format!("Leading '+' is not supported."),
UnexpectedBinaryInitial => format!("Unexpected binary operator in initial position."),
UnmatchedCloseParen => format!("What's this ')' doing here? I don't see a '('"),
UnexpectedEOF => format!("Hold your horses. An EOF already?"),
GeneralUnexpected => format!("Unexpected token."),
ParamExpComma => format!("Expected ',' after function parameter."),
CallExpComma => format!("Expected ',' after function argument."),
CallExpCloseParen => format!("Expected ')' to end function arguments."),
FnBadComma => format!("Comma ',' is not allowed before first argument."),
ExpectedSemi => format!("Expected semicolon to end the statement"),
FnExpectedName => {
    format!("Expected identifier. Functions must have a name. {EXAMPLE_FN}")
},
LetExpName => {
    format!("Expected identifier. A 'let' statement must assign to a name. {EXAMPLE_LET}")
},
ComparatorChainDisallowed => {
    "Chaining comparison operations is not yet supported.".to_string()
},
IfExpOpenParen => {
    format!("Expected '(' for condition of 'if' statement. {EXAMPLE_IF}")
},
IfExpCloseParen => {
    format!("Expected ')' to close condition of 'if' statement. {EXAMPLE_IF}")
},
ExpType => {
    "Expected a type here. Try 'i64' or '()'.".to_string()
},
AssignLHSMustBeIdent => {
    format!("Left-hand-side of assignment must be an identifier, not a composite expression. {EXAMPLE_ASSIGN}")
},
}

def_token_errors! {
pub(crate) struct FnExpParameter {
    pub(crate) fn_name: Ident,
}
msg: self => format!(
    "Expected an identifier to serve as a function parameter for function '{0}'. {EXAMPLE_FN}",
    self.fn_name
);

pub(crate) struct FnExpOpenParen {
    pub(crate) fn_name: Ident,
}
msg: self => format!(
    "Expected '(' to begin parameter declaration for function '{}'. {EXAMPLE_FN}",
    self.fn_name
);

pub(crate) struct FnExpColon {
    pub(crate) fn_name: Ident,
    pub(crate) param: Ident,
}
msg: self => format!(
    "Expected ':' to provide the type of the parameter '{1}' \
    for function '{0}'. {EXAMPLE_FN}",
    self.fn_name, self.param
);

pub(crate) struct InvalidToken {
    pub(crate) token: token::InvalidToken,
}
msg: self => format!("{}", self.token.msg);

pub(crate) struct FnExpComma {
    pub(crate) fn_name: Ident,
    pub(crate) param_name: Ident,
}
msg: self => format!(
    "Expected ',' after function parameter '{1}' for function '{0}'. {EXAMPLE_FN}",
    self.fn_name, self.param_name
);

pub(crate) struct FnExpCloseParen {
    pub(crate) fn_name: Ident,
}
msg: self => format!(
    "Expected ')' to end function parameters for function '{0}'. {EXAMPLE_FN}",
    self.fn_name
);

pub(crate) struct FnExpThinArrow {
    pub(crate) fn_name: Ident,
}
msg: self => format!(
    "Expected '->' to declare return type for function '{0}'. {EXAMPLE_FN}",
    self.fn_name
);

pub(crate) struct FnExpReturnType {
    pub(crate) fn_name: Ident,
}
msg: self => format!(
    "Expected an identifier to provide the return type for function '{0}'. {EXAMPLE_FN}",
    self.fn_name
);

pub(crate) struct FnExpOpenCurly {
    pub(crate) fn_name: Ident,
}
msg: self => format!(
    "Expected '{{' to begin the body of function '{0}'. {EXAMPLE_FN}",
    self.fn_name
);

pub(crate) struct FnExpCloseCurly {
    pub(crate) fn_name: Ident,
}
msg: self => format!(
    "Expected '}}' to end the body of function '{0}'. {EXAMPLE_FN}",
    self.fn_name
);

pub(crate) struct BlockExpCloseCurly {
    pub(crate) open_curly: Span,
}
msg: self => format!(
    "Expected '}}' to end the body of block started at '{0:?}'",
    self.open_curly
);

pub(crate) struct LetExpEquals {
    pub(crate) ident: Ident,
}
msg: self => format!(
    "Expected an '=', to provide an initial value for '{0}'. {EXAMPLE_LET}",
self.ident);
}
