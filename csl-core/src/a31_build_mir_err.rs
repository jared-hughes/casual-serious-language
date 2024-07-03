use crate::ast::*;
use crate::errors::{def_errors, Diag, Diagnostic};
use crate::span::Span;
use crate::types::Type;

pub(crate) struct ArgCountWrong {
    pub(crate) fn_name: String,
    pub(crate) count_expected: usize,
    pub(crate) count_actual: usize,
}

pub(crate) fn from_arg_count(span: Span, acw: ArgCountWrong) -> Diag {
    if acw.count_actual > acw.count_expected {
        TooManyArgs { span, acw }.into_diag()
    } else {
        TooFewArgs { span, acw }.into_diag()
    }
}

def_errors! {
pub(crate) struct IdentifierNotFound {
    /// The span of the identifier reference
    pub(crate) span: Span,
    /// Name of identifier
    pub(crate) name: String,
}
msg: self => format!(
    "Identifier '{0}' not found in the local scope of the function.",
    self.name
);

pub(crate) struct FunctionNotFound {
    /// Span of function name
    pub(crate) span: Span,
    /// Name of function
    pub(crate) name: String,
}
msg: self => format!(
    "Function '{0}' not found. Try defining it with \
    `fn {0}(x: u64) -> u64 {{ ret x * x; }}`",
    self.name
);

pub(crate) struct LocalImmutable {
    /// The span of the identifier reference
    pub(crate) span: Span,
    /// Name of identifier
    pub(crate) name: String,
}
msg: self => format!(
    "Identifier '{0}' is immutable. Try declaring it with 'let mut {0}' instead.",
    self.name
);

pub(crate) struct CallNotIdent {
    /// Span of function name
    pub(crate) span: Span,
}
msg: self => format!("Function name must be an identifier.");

pub(crate) struct InvalidTypeUnary {
    // TODO-errormsg: UnaryOp already has span
    pub(crate) span: Span,
    pub(crate) op: UnaryOp,
    pub(crate) arg_type: Type,
}
msg: self => format!("Type error: Cannot perform {0}{1}", self.op.node, self.arg_type);

pub(crate) struct InvalidTypeBinary {
    // TODO-errormsg: BinOp already has span
    pub(crate) span: Span,
    pub(crate) op: BinOp,
    pub(crate) left_type: Type,
    pub(crate) right_type: Type,
}
msg: self => format!(
    "Type error: Cannot perform {} {} {}",
        self.left_type, self.op.node, self.right_type
);

pub(crate) struct FnInExpr{
    /// Span of function name
    pub(crate) span: Span,
}
msg: self => format!(
    "Function expressions are not yet supported. Try moving this to be a global function."
);

pub(crate) struct TopLevelExpr {
    pub(crate) span: Span
}
msg: self => format!(
    "Top-level exprs are not yet supported. \
    Try a main function instead: `fn main() -> i64 {{ ret 1 + 2; }}`"
);

pub(crate) struct UnrecognizedTypeName {
    pub(crate) span: Span,
    pub(crate) name: String
}
msg: self => format!(
    "Type '{}' is not recognized. Try 'i64' or 'f64' instead.",
    self.name
);

pub(crate) struct MisplacedLet {
    pub(crate) span: Span
}
msg: self => format!(
    "Misplaced 'let'. For now, 'let' can only be used in statement position."
);

pub(crate) struct MisplacedRet {
    pub(crate) span: Span
}
msg: self => format!(
        "Misplaced 'ret'. The keyword 'ret' can only be \
        applied to the final statement in a block."
    );

pub(crate) struct TooManyArgs {
    pub(crate) span: Span,
    pub(crate) acw: ArgCountWrong
}
msg: self => format!(
        "Too many args for function '{0}': expected {1} but got {2}.",
        self.acw.fn_name, self.acw.count_expected, self.acw.count_actual
    );

pub(crate) struct TooFewArgs {
    pub(crate) span: Span,
    pub(crate) acw: ArgCountWrong
}
msg: self => format!(
        "Too few args for function '{0}': expected {1} but got only {2}.",
        self.acw.fn_name, self.acw.count_expected, self.acw.count_actual
    );

pub(crate) struct WrongArgType {
    pub(crate) span: Span,
    pub(crate) fn_name: String,
    pub(crate) actual: Type,
    pub(crate) expected: Type
}
msg: self => format!(
        "Function '{0}' expected an argument of type '{1}' here, \
        but you passed in '{2}'",
        self.fn_name, self.expected, self.actual
    );

pub(crate) struct WrongReturnType {
    pub(crate) span: Span,
    pub(crate) fn_name: String,
    pub(crate) actual: Type,
    pub(crate) expected: Type
}
msg: self => format!(
        "Expected function '{0}' to return type '{1}', \
        but it returned type '{2}'",
        self.fn_name, self.expected, self.actual
    );

pub(crate) struct DuplicateDefinition {
    pub(crate) span: Span,
    pub(crate) name: String
}
msg: self => format!("Cannot redefine '{0}' since it is already defined.", self.name);

pub(crate) struct DuplicateParameter {
    pub(crate) span: Span,
    pub(crate) name: String
}
msg: self => format!("Duplicate parameter '{0}' already defined.", self.name);

pub(crate) struct DuplicateFnName {
    pub(crate) span: Span,
    pub(crate) name: String
}
msg: self => format!("Duplicate function '{0}' already defined.", self.name);

pub(crate) struct WrongElseType {
    pub(crate) span: Span,
    pub(crate) if_type: Type,
    pub(crate) else_type: Type,
}
msg: self => format!(
        "Expected else branch to return type '{0}' (the same type as the 'if' branch), \
        but the else branch returned type '{1}'.",
        self.if_type, self.else_type
    );


pub(crate) struct WrongTypeMissingElse {
    pub(crate) span: Span,
    pub(crate) if_type: Type,
}
msg: self => format!(
        "Expected else branch to return type '{0}' (the same type as this 'if' branch), \
        but there is no else branch. Try removing 'ret' from the 'if' branch, or adding an 'else' branch.",
        self.if_type
    );

pub(crate) struct WrongIfConditionType {
    pub(crate) span: Span,
    pub(crate) cond_type: Type,
}
msg: self => format!(
        "Expected 'if' to branch on a boolean ('Bool') but got type '{0}'.",
        self.cond_type
    );

pub(crate) struct WrongAndOrConditionType {
    pub(crate) span: Span,
    pub(crate) cond_type: Type,
    /// Kind should either be `And` or `Or`
    pub(crate) kind: BinOpKind
}
msg: self => format!(
        "Expected first argument of '{1}' to be 'Bool' but got type '{0}'.",
        self.cond_type, self.kind
    );

pub(crate) struct WrongAndOrSecondType {
    pub(crate) span: Span,
    pub(crate) right_type: Type,
    /// Kind should either be `And` or `Or`
    pub(crate) kind: BinOpKind
}
msg: self => format!(
        "Expected second argument of '{1}' to be 'Bool' but got type '{0}'.",
        self.right_type, self.kind
    );

pub(crate) struct NotAType {
    pub(crate) span: Span,
}
msg: self => format!(
    "This is not a valid representation of a type. Try something like 'i64' or '()' instead."
);

}
