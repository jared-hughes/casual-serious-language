use crate::span::{Span, Spanned};
pub use crate::token::Lit;
use std::fmt;

pub use BinOpKind::*;
#[derive(Clone, Copy, Debug)]
pub enum BinOpKind {
    /// Addition (`+`)
    Add,
    /// Subtraction (`-`)
    Sub,
    /// Multiplication (`*`)
    Mul,
    /// Division (`/`)
    Div,
}

impl fmt::Display for BinOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
        };
        write!(f, "{}", s)
    }
}

pub type BinOp = Spanned<BinOpKind>;

pub use UnaryOpKind::*;
#[derive(Clone, Copy, Debug)]
pub enum UnaryOpKind {
    /// Negation (`-`)
    Neg,
}

impl fmt::Display for UnaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Neg => "-",
        };
        write!(f, "{}", s)
    }
}

pub type UnaryOp = Spanned<UnaryOpKind>;

#[derive(Clone, Debug)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

pub struct FunctionParam {
    pub name: Ident,
    pub param_type: Ident,
}

pub struct FunctionDefinition {
    pub fn_name: Ident,
    pub params: Vec<FunctionParam>,
    pub body: Vec<Expr>,
    pub return_type: Ident,
}

pub use ExprInner::*;
pub enum ExprInner {
    /// One-parameter operation like `-x`
    Unary(UnaryOp, Box<Expr>),
    /// Binary operation like `a + b`
    Binary(BinOp, Box<Expr>, Box<Expr>),
    /// Literal, such as a number `1`.
    Literal(Lit),
    /// Identifier like `x`.
    // TODO: store these as indexes to learn about interning and arenas
    // See Rust's InternerInner in rustc_span.
    Ident(String),
    /// Function definition
    FnDefinition(FunctionDefinition),
    /// Return with unchanged control flow, `ret x;`
    Ret(Span, Box<Expr>),
    /// Parenthesized expression `(x)`.
    Paren(Box<Expr>),
    /// Function call `f(x,y,z)`
    FnCall(Box<Expr>, Vec<Expr>),
}

pub struct Expr {
    pub body: ExprInner,
    pub span: Span,
}

impl Expr {
    pub fn new(body: ExprInner, span: Span) -> Expr {
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
            Ident(x) => write!(f, "Ident({x:?})"),
            FnDefinition(def) => {
                write!(f, "FnDefinition[{}](", def.fn_name)?;
                for (i, p) in (&def.params).into_iter().enumerate() {
                    write!(f, "{}: {}", p.name, p.param_type)?;
                    if i < def.params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") ")?;
                f.debug_set().entries(&def.body).finish()
            }
            Ret(span, arg) => {
                write!(f, "ret({:?}) ", span)?;
                f.debug_tuple("").field(arg).finish()
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
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:?}){:#?}", self.span, self.body)
    }
}

pub struct Program {
    pub body: Vec<Expr>,
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in &self.body {
            writeln!(f, "{:?}", stmt)?;
        }
        Ok(())
    }
}