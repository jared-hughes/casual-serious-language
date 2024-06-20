pub use crate::token::Lit;
use std::fmt;

pub use BinOp::*;
#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    /// Addition (`+`)
    Add,
    /// Subtraction (`-`)
    Sub,
    /// Multiplication (`*`)
    Mul,
    /// Division (`/`)
    Div,
}

pub use Expr::*;
pub enum Expr {
    /// Binary operation like `a + b`
    Binary(BinOp, Box<Expr>, Box<Expr>),
    /// Literal, such as a number `1`.
    Literal(Lit),
    /// Identifier like `x`.
    // TODO: store these as indexes to learn about interning and arenas
    // See Rust's InternerInner in rustc_span.
    Ident(String),
}

impl fmt::Debug for Expr {
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
