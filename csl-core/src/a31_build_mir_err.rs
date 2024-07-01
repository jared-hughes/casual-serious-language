use crate::ast::*;
use crate::errors::{def_errors, Diag, Diagnostic};
use crate::span::Span;
use crate::types::Type;

pub struct ArgCountWrong {
    pub fn_name: String,
    pub count_expected: usize,
    pub count_actual: usize,
}

pub fn from_arg_count(span: Span, acw: ArgCountWrong) -> Diag {
    if acw.count_actual > acw.count_expected {
        TooManyArgs { span, acw }.into_diag()
    } else {
        TooFewArgs { span, acw }.into_diag()
    }
}

def_errors! {
pub struct IdentifierNotFound {
    /// The span of the identifier reference
    pub span: Span,
    /// Name of identifier
    pub name: String,
}
msg: self => format!(
    "Identifier '{0}' not found in the local scope of the function.",
    self.name
);

pub struct FunctionNotFound {
    /// Span of function name
    pub span: Span,
    /// Name of function
    pub name: String,
}
msg: self => format!(
    "Function '{0}' not found. Try defining it with \
    `fn {0}(x: u64) -> u64 {{ ret x * x; }}`",
    self.name
);

pub struct CallNotIdent {
    /// Span of function name
    pub span: Span,
}
msg: self => format!("Function name must be an identifier.");

pub struct InvalidTypeUnary {
    // TODO-errormsg: UnaryOp already has span
    pub span: Span,
    pub op: UnaryOp,
    pub arg_type: Type,
}
msg: self => format!("Type error: Cannot perform {0}{1}", self.op.node, self.arg_type);

pub struct InvalidTypeBinary {
    // TODO-errormsg: BinOp already has span
    pub span: Span,
    pub op: BinOp,
    pub left_type: Type,
    pub right_type: Type,
}
msg: self => format!(
    "Type error: Cannot perform {} {} {}",
        self.left_type, self.op.node, self.right_type
);

pub struct FnInExpr{
    /// Span of function name
    pub span: Span,
}
msg: self => format!(
    "Function expressions are not yet supported. Try moving this to be a global function."
);

pub struct TopLevelExpr {
    pub span: Span
}
msg: self => format!(
    "Top-level exprs are not yet supported. \
    Try a main function instead: `fn main() -> i64 {{ ret 1 + 2; }}`"
);

pub struct UnrecognizedTypeName {
    pub span: Span,
    pub name: String
}
msg: self => format!(
    "Type '{}' is not recognized. Try 'i64' or 'f64' instead.",
    self.name
);

pub struct MisplacedLet {
    pub span: Span
}
msg: self => format!(
    "Misplaced 'let'. For now, 'let' can only be used in statement position."
);

pub struct MisplacedRet {
    pub span: Span
}
msg: self => format!(
        "Misplaced 'ret'. The keyword 'ret' can only be \
        applied to the final statement in a block."
    );

pub struct FnMissingRet {
    pub span: Span
}
msg: self => format!(
        "Unit type '()' has not yet been implemented, so \
        all functions must have a return. Try adding 'ret'."
    );

pub struct BlockMissingRet {
    pub span: Span
}
msg: self => format!(
        "Unit type '()' has not yet been implemented, so \
        all blocks must have a return. Try adding 'ret'."
    );

pub struct TooManyArgs {
    pub span: Span,
    pub acw: ArgCountWrong
}
msg: self => format!(
        "Too many args for function '{0}': expected {1} but got {2}.",
        self.acw.fn_name, self.acw.count_expected, self.acw.count_actual
    );

pub struct TooFewArgs {
    pub span: Span,
    pub acw: ArgCountWrong
}
msg: self => format!(
        "Too few args for function '{0}': expected {1} but got only {2}.",
        self.acw.fn_name, self.acw.count_expected, self.acw.count_actual
    );

pub struct WrongArgType {
    pub span: Span,
    pub fn_name: String,
    pub actual: Type,
    pub expected: Type
}
msg: self => format!(
        "Function '{0}' expected an argument of type '{1}' here, \
        but you passed in '{2}'",
        self.fn_name, self.expected, self.actual
    );

pub struct WrongReturnType {
    pub span: Span,
    pub fn_name: String,
    pub actual: Type,
    pub expected: Type
}
msg: self => format!(
        "Expected function '{0}' to return type '{1}', \
        but it returned type '{2}'",
        self.fn_name, self.expected, self.actual
    );

pub struct DuplicateDefinition {
    pub span: Span,
    pub name: String
}
msg: self => format!("Cannot redefine '{0}' since it is already defined.", self.name);

pub struct DuplicateParameter {
    pub span: Span,
    pub name: String
}
msg: self => format!("Duplicate parameter '{0}' already defined.", self.name);

pub struct DuplicateFnName {
    pub span: Span,
    pub name: String
}
msg: self => format!("Duplicate function '{0}' already defined.", self.name);

pub struct WrongElseType {
    pub span: Span,
    pub if_type: Type,
    pub else_type: Type,
}
msg: self => format!(
        "Expected else branch to return type '{0}' (the same type as the 'if' branch), \
        but the else branch returned type '{1}'.",
        self.if_type, self.else_type
    );

pub struct WrongIfConditionType {
    pub span: Span,
    pub cond_type: Type,
}
msg: self => format!(
        "Expected 'if' to branch on a boolean ('Bool') but got type '{0}'.",
        self.cond_type
    );


}
