use crate::pos::ByteLen;
use crate::span::Span;
use std::fmt;

pub use Lit::*;
#[derive(Debug, Clone, Copy)]
pub enum Lit {
    /// e.g. `1`, `1e3`
    Integer(i64),
    // e.g. `1.`, `1.0`, `1.0e3`
    Float(f64),
}

pub use BinOpToken::*;
#[derive(Debug, Clone, Copy)]
pub enum BinOpToken {
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
}

pub use Delimiter::*;
#[derive(Debug, Clone, Copy)]
pub enum Delimiter {
    /// `( ... )`
    Parenthesis,
}

pub use TokenKind::*;
#[derive(Debug, Clone, Copy)]
pub enum TokenKind<'a> {
    /* Operators */
    BinOp(BinOpToken),

    /* Punctuation */
    /// An opening delimiter, like `(`.
    OpenDelim(Delimiter),
    /// A closing delimiter, like `)`.
    CloseDelim(Delimiter),

    /* Atoms */
    /// Literal number
    Literal(Lit),
    /// Identifier
    Ident(&'a str),

    /* End of File */
    Eof,
    Whitespace,
}

#[derive(Clone, Copy)]
pub struct TokenLen<'a> {
    pub kind: TokenKind<'a>,
    pub len: ByteLen,
}

impl<'a> TokenLen<'a> {
    pub fn new(kind: TokenKind<'a>, len: ByteLen) -> TokenLen<'a> {
        TokenLen { kind, len }
    }
}

impl<'a> fmt::Debug for TokenLen<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} [len={}]", self.kind, u32::from(self.len))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind<'a>, span: Span) -> Token<'a> {
        Token { kind, span }
    }
}
