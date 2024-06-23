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

pub type BinOp = Spanned<BinOpKind>;

pub use ExprInner::*;
pub enum ExprInner {
    /// Binary operation like `a + b`
    Binary(BinOp, Box<Expr>, Box<Expr>),
    /// Literal, such as a number `1`.
    Literal(Lit),
    /// Identifier like `x`.
    // TODO: store these as indexes to learn about interning and arenas
    // See Rust's InternerInner in rustc_span.
    Ident(String),
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
            Self::Binary(op, left, right) => {
                // `{:?}` never has spacing but `{:#?}` puts newlines and indents.
                write!(f, "Binary[{op:?}]")?;
                f.debug_tuple("").field(left).field(right).finish()
            }
            Self::Literal(x) => write!(f, "Literal({x:?})"),
            Self::Ident(x) => write!(f, "Ident({x:?})"),
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:?}){:#?}", self.span, self.body)
    }
}
