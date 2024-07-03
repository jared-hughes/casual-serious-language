use crate::span::{Span, Spanned};
pub(crate) use crate::token::Lit;
use std::fmt;

pub(crate) use CompareOpKind::*;
#[derive(Clone, Copy, Debug)]
pub(crate) enum CompareOpKind {
    /// `<`
    Lt,
    /// `<=`
    LtEq,
    /// `>`
    Gt,
    /// `>=`
    GtEq,
    /// `!=`
    Neq,
    /// `==`
    Eq,
}

pub(crate) use BinOpKind::*;
#[derive(Clone, Copy, Debug)]
pub(crate) enum BinOpKind {
    Compare(CompareOpKind),
    /// Boolean OR (`||`)
    Or,
    /// Boolean AND (`&&`)
    And,
    /// Addition (`+`)
    Add,
    /// Subtraction (`-`)
    Sub,
    /// Multiplication (`*`)
    Mul,
    /// True division (`/`)
    TrueDiv,
    /// Floor division (`//`)
    FloorDiv,
}

impl fmt::Display for BinOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Or => "||",
            And => "&&",
            Add => "+",
            Sub => "-",
            Mul => "*",
            TrueDiv => "/",
            FloorDiv => "/",
            Compare(Lt) => "<",
            Compare(LtEq) => "<=",
            Compare(Gt) => ">",
            Compare(GtEq) => ">=",
            Compare(Neq) => "!=",
            Compare(Eq) => "==",
        };
        write!(f, "{}", s)
    }
}

pub(crate) type BinOp = Spanned<BinOpKind>;

pub(crate) use UnaryOpKind::*;
#[derive(Clone, Copy, Debug)]
pub(crate) enum UnaryOpKind {
    /// Negation (`-`)
    Neg,
    /// Not (`!`)
    Not,
}

impl fmt::Display for UnaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Neg => "-",
            Not => "!",
        };
        write!(f, "{}", s)
    }
}

pub(crate) type UnaryOp = Spanned<UnaryOpKind>;

#[derive(Clone)]
pub(crate) struct Ident {
    pub(crate) name: String,
    pub(crate) span: Span,
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:?})Ident({:?})", self.span, self.name)
    }
}

pub(crate) struct FunctionParam {
    pub(crate) name: Ident,
    pub(crate) param_type: Box<Expr>,
}

pub(crate) struct FunctionDefinition {
    pub(crate) fn_name: Ident,
    pub(crate) params: Vec<FunctionParam>,
    pub(crate) body: Vec<Statement>,
    pub(crate) return_type: Box<Expr>,
}

pub(crate) use ExprInner::*;
pub(crate) enum ExprInner {
    /// One-parameter operation like `-x`
    Unary(UnaryOp, Box<Expr>),
    /// Binary operation like `a + b`
    Binary(BinOp, Box<Expr>, Box<Expr>),
    /// Literal, such as a number `1`.
    Literal(Lit),
    /// Identifier like `x`.
    IdentExpr(String),
    /// Function definition
    FnDefinition(FunctionDefinition),
    /// Block `y = { ret x + 1; }`
    Block(Vec<Statement>),
    /// Parenthesized expression `(x)`.
    Paren(Box<Expr>),
    /// Function call `f(x,y,z)`
    FnCall(Box<Expr>, Vec<Expr>),
    /// If expression `if (x > 0) 1 else 0`
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    /// Assignment expression `x = 3`
    Assign(Ident, Box<Expr>),
}

pub(crate) struct Expr {
    pub(crate) body: ExprInner,
    pub(crate) span: Span,
}

impl Expr {
    pub(crate) fn new(body: ExprInner, span: Span) -> Expr {
        Expr { body, span }
    }
}

impl fmt::Debug for ExprInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Unary(op, arg) => {
                write!(f, "Unary[{op:?}]")?;
                f.debug_tuple("").field(arg).finish()
            }
            Binary(op, left, right) => {
                // `{:?}` never has spacing but `{:#?}` puts newlines and indents.
                write!(f, "Binary[{op:?}]")?;
                f.debug_tuple("").field(left).field(right).finish()
            }
            Literal(x) => write!(f, "Literal({x:?})"),
            IdentExpr(x) => write!(f, "Ident({x:?})"),
            FnDefinition(def) => {
                write!(f, "FnDefinition[{}](", def.fn_name)?;
                for (i, p) in def.params.iter().enumerate() {
                    write!(f, "{}: {:?}", p.name, p.param_type)?;
                    if i < def.params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> {:?} ", def.return_type)?;
                f.debug_set().entries(&def.body).finish()
            }
            Block(exprs) => {
                write!(f, "Block")?;
                f.debug_set().entries(exprs).finish()
            }
            Paren(arg) => write!(f, "paren@{:#?}", arg),
            FnCall(fun, args) => {
                write!(f, "call({:?})", fun)?;
                let mut tup = f.debug_tuple("");
                for arg in args {
                    tup.field(arg);
                }
                tup.finish()
            }
            If(cond, true_branch, false_branch) => f
                .debug_struct("If")
                .field("cond", cond)
                .field("true", true_branch)
                .field("false", false_branch)
                .finish(),
            Assign(ident, expr) => f
                .debug_struct("Assign")
                .field("ident", ident)
                .field("expr", expr)
                .finish(),
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:?}){:#?}", self.span, self.body)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Mutability {
    Yes,
    No,
}
impl Mutability {
    fn prefix(&self) -> &str {
        match self {
            Mutability::Yes => "mut ",
            Mutability::No => "",
        }
    }
}

pub(crate) struct LetLHS {
    pub(crate) ident: Ident,
    pub(crate) mutability: Mutability,
}

impl fmt::Debug for LetLHS {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.mutability.prefix(), self.ident)
    }
}

pub(crate) use StatementKind::*;
pub(crate) enum StatementKind {
    /// `let x = 5;`
    Let(LetLHS),
    /// `ret 5`
    Ret,
    /// `5`
    Bare,
}

pub(crate) struct Statement {
    pub(crate) span: Span,
    pub(crate) kind: StatementKind,
    pub(crate) expr: Box<Expr>,
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            Let(lhs) => write!(f, "({:?}) let {:?} = ", self.span, lhs)?,
            Ret => write!(f, "({:?}) ret ", self.span)?,
            Bare => {}
        }
        write!(f, "{:?}", self.expr)
    }
}

pub(crate) struct Program {
    pub(crate) body: Vec<Statement>,
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in &self.body {
            writeln!(f, "{:?}", stmt)?;
        }
        Ok(())
    }
}
