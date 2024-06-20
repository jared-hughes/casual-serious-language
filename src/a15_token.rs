pub use Lit::*;
#[derive(Debug, Clone, Copy)]
pub enum Lit {
    /// e.g. `1`, `1e3`
    Integer(u32),
    // e.g. `1.`, `1.0`, `1.0e3`
    // Float,
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

pub use Token::*;
#[derive(Debug, Clone, Copy)]
pub enum Token<'a> {
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
}
