use crate::pos::ByteLen;
use crate::span::Span;
use std::fmt;

pub(crate) use Lit::*;
#[derive(Debug, Clone, Copy)]
pub(crate) enum Lit {
    /// e.g. `1`, `1e3`
    Integer(i64),
    /// e.g. `1.`, `1.0`, `1.0e3`
    Float(f64),
    /// `()`
    Unit,
}

pub(crate) use BinOpToken::*;
#[derive(Debug, Clone, Copy)]
pub(crate) enum BinOpToken {
    /// `&&`
    And,
    /// `||`
    Or,
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
    EqEq,
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
}

pub(crate) use Delimiter::*;
#[derive(Debug, Clone, Copy)]
pub(crate) enum Delimiter {
    /// `( ... )`
    Parenthesis,
    /// `{ ... }`
    CurlyBrace,
}

pub(crate) use Keyword::*;
#[derive(Debug, Clone, Copy)]
pub(crate) enum Keyword {
    /// 'fn'
    Fn,
    /// 'ret'
    Ret,
    /// 'let'
    Let,
    /// 'if'
    If,
    /// 'else'
    Else,
}

pub(crate) use TokenKind::*;
#[derive(Debug, Clone, Copy)]
pub(crate) enum TokenKind<'a> {
    /* Operators */
    BinOp(BinOpToken),

    /* Punctuation */
    /// An opening delimiter, like `(`.
    OpenDelim(Delimiter),
    /// A closing delimiter, like `)`.
    CloseDelim(Delimiter),
    /// '='
    Equals,
    /// ';'
    Semi,
    /// ':'
    Colon,
    /// ','
    Comma,
    /// '->'
    ThinArrow,
    /// '!'
    Bang,

    /* Keywords */
    Kw(Keyword),

    /* Atoms */
    /// Literal number
    Literal(Lit),
    /// Identifier
    Ident(&'a str),

    /* Unprintables */
    Eof,
    Whitespace,
    Invalid(InvalidToken),
}

// TODO: more standard error handling here.
#[derive(Debug, Clone, Copy)]
pub(crate) struct InvalidToken {
    pub(crate) msg: &'static str,
}

#[derive(Clone, Copy)]
pub(crate) struct TokenLen<'a> {
    pub(crate) kind: TokenKind<'a>,
    pub(crate) len: ByteLen,
}

impl<'a> TokenLen<'a> {
    pub(crate) fn new(kind: TokenKind<'a>, len: ByteLen) -> TokenLen<'a> {
        TokenLen { kind, len }
    }
}

impl<'a> fmt::Debug for TokenLen<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} [len={}]", self.kind, u32::from(self.len))
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Token<'a> {
    pub(crate) kind: TokenKind<'a>,
    pub(crate) span: Span,
}

impl<'a> Token<'a> {
    pub(crate) fn new(kind: TokenKind<'a>, span: Span) -> Token<'a> {
        Token { kind, span }
    }
}
