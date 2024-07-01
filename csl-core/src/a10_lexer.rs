use crate::pos::{ByteLen, BytePos};
use crate::span::span;
use crate::token::*;
use std::str::{Chars, FromStr};

pub struct Lexer<'a> {
    lexer: RawLexer<'a>,
    pos: BytePos,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            lexer: RawLexer::new(input),
            pos: BytePos(0),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        loop {
            let token = self.lexer.next_raw_token();
            let lo = self.pos;
            match token.kind {
                // Just update position
                Whitespace => {
                    self.pos = lo + token.len;
                    continue;
                }
                // Common case
                _ => {
                    let hi = lo + token.len;
                    self.pos = hi;
                    return Some(Token::new(token.kind, span(lo, hi)));
                }
            };
        }
    }
}

/// Before any lexing, or after finishing a RawToken, `chars` points to the
/// first character that has not been in a token yet.
/// In the middle of finding the extent of a RawToken, `chars_token_start`
/// points to the first character that will be in this token.
pub struct RawLexer<'a> {
    /// Iterator pointing at the current character
    chars: Chars<'a>,
    /// Iterator pointing at the character which starts this token
    chars_token_start: Chars<'a>,
}

const EOF_CHAR: char = '\0';

impl<'a> Iterator for RawLexer<'a> {
    type Item = TokenLen<'a>;

    fn next(&mut self) -> Option<TokenLen<'a>> {
        let token = self.next_raw_token();
        return match token.kind {
            // Stop the iterator
            Eof => None,
            // Common case
            _ => Some(token),
        };
    }
}

impl<'a> RawLexer<'a> {
    pub fn new(input: &'a str) -> RawLexer<'a> {
        RawLexer {
            chars: input.chars(),
            chars_token_start: input.chars(),
        }
    }

    /// Return the char that `chars` points to, and shift it along.
    fn consume(&mut self) -> Option<char> {
        self.chars.next()
    }

    /// Peeks the next symbol from the input stream without consuming it.
    /// If requested position doesn't exist, `EOF_CHAR` is returned.
    /// However, getting `EOF_CHAR` doesn't always mean actual end of file,
    /// it should be checked with `is_eof` method. (It could be a literal null
    /// byte in the file, but that won't affect the matching on char).
    fn peek(&self) -> char {
        // `.next()` optimizes better than `.nth(0)` according to Rust repo.
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    /// Checks if there is nothing more to consume.
    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    /// Eats symbols while predicate returns true, or until the end of file is reached.
    /// After calling this method, `chars` will point to one after the last
    /// character satisfying the predicate.
    fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        // It was tried making optimized version of this for e.g. line comments, but
        // LLVM can inline all of this and compile it down to fast iteration over bytes.
        while predicate(self.peek()) && !self.is_eof() {
            self.consume();
        }
    }
}

impl<'a> RawLexer<'a> {
    /** Length of the current token, in bytes. */
    fn len(&self) -> ByteLen {
        let start = self.chars_token_start.as_str();
        ByteLen::from(start.len() - self.chars.as_str().len())
    }

    /** Borrowed string slice containing all chars of the current token. */
    fn slice(&self) -> &'a str {
        let start = self.chars_token_start.as_str();
        let num_bytes = start.len() - self.chars.as_str().len();
        return &start[0..num_bytes];
    }

    /// Parses a token from the input string, including whitespace.
    fn next_raw_token(&mut self) -> TokenLen<'a> {
        let first_char = match self.consume() {
            Some(c) => c,
            None => return TokenLen::new(Eof, ByteLen(0)),
        };
        let kind = match first_char {
            c if is_whitespace(c) => {
                self.eat_while(is_whitespace);
                Whitespace
            }

            c if is_id_start(c) => self.ident(),

            // Numeric literal.
            '0'..='9' => self.number(),

            '+' => BinOp(Plus),
            '-' => match self.peek() {
                '>' => {
                    self.consume();
                    ThinArrow
                }
                _ => BinOp(Minus),
            },
            '*' => BinOp(Star),
            '/' => BinOp(Slash),
            '(' => OpenDelim(Parenthesis),
            ')' => CloseDelim(Parenthesis),
            '{' => OpenDelim(CurlyBrace),
            '}' => CloseDelim(CurlyBrace),
            ',' => Comma,
            ':' => Colon,
            ';' => Semi,
            '!' => match self.peek() {
                '=' => {
                    self.consume();
                    BinOp(Neq)
                }
                _ => Bang,
            },
            '=' => match self.peek() {
                '=' => {
                    self.consume();
                    BinOp(EqEq)
                }
                '>' => {
                    self.consume();
                    invalid("Greater-than-equals is written as '>=', not '=>'.")
                }
                '<' => {
                    self.consume();
                    invalid("Less-than-equals is written as '<=', not '=<'.")
                }
                _ => Equals,
            },
            '<' => match self.peek() {
                '=' => {
                    self.consume();
                    BinOp(LtEq)
                }
                '<' => {
                    self.consume();
                    invalid("Bit-shift-left ('<<') is not yet supported.")
                }
                _ => BinOp(Lt),
            },
            '>' => match self.peek() {
                '=' => {
                    self.consume();
                    BinOp(GtEq)
                }
                '>' => {
                    self.consume();
                    invalid("Bit-shift-right ('>>') is not yet supported.")
                }
                _ => BinOp(Gt),
            },
            '&' => match self.peek() {
                '&' => {
                    self.consume();
                    BinOp(And)
                }
                _ => invalid("Bitwise AND ('&') is not yet supported."),
            },
            '|' => match self.peek() {
                '|' => {
                    self.consume();
                    BinOp(Or)
                }
                _ => invalid("Bitwise OR ('|') is not yet supported."),
            },

            // TODO: actually say what the character is.
            _ => invalid("Unrecognized character."),
        };
        let len = self.len();
        self.chars_token_start = self.chars.clone();
        TokenLen::new(kind, len)
    }

    fn ident(&mut self) -> TokenKind<'a> {
        self.eat_while(is_id_continue);
        let s = self.slice();
        match () {
            () if s == "fn" => KwFn,
            () if s == "ret" => KwRet,
            () if s == "let" => KwLet,
            () if s == "if" => KwIf,
            () if s == "else" => KwElse,
            _ => Ident(s),
        }
    }

    fn number(&mut self) -> TokenKind<'a> {
        self.eat_while(is_digit);
        // consumed so far: `[digits]`
        match self.peek() {
            '.' => {
                self.consume();
                let saw_digits = self.eat_decimal_digits();
                if !saw_digits {
                    return invalid("Need at least one digit after '.'.");
                }
                // consumed so far: `[digits].[digits]`
                if self.peek() == 'e' {
                    self.consume();
                    if self.peek() == '-' {
                        self.consume();
                    }
                    // consumed so far: `[digits].[digits]e` with optional `-`.
                    let saw_digits = self.eat_decimal_digits();
                    if !saw_digits {
                        return invalid("Need at least one digit after 'e'.");
                    }
                }
                // consumed so far: `[digits].[digits]` or `[digits].[digits]e-?digits`
                match f64::from_str(self.slice()) {
                    Ok(x) => Literal(Float(x)),
                    // Should be unreachable:
                    Err(_) => invalid("Unparseable float."),
                }
            }
            'e' => {
                let len_before_e = self.len().0 as usize;
                self.consume();
                if self.peek() == '-' {
                    self.consume();
                }
                // consumed so far: `[digits]e` with optional `-`.
                let saw_digits = self.eat_decimal_digits();
                if !saw_digits {
                    return invalid("Need at least one digit after 'e'.");
                }
                let significand = &self.slice()[0..len_before_e];
                let exponent = &self.slice()[len_before_e + 1..];
                match (i64::from_str(significand), i64::from_str(exponent)) {
                    (Ok(s), Ok(ex)) => {
                        if ex < 0 {
                            return invalid("Integer exponent cannot be negative. Use '.' if you want a float literal.");
                        } else if ex > 20 {
                            return invalid("Integer exponent is much too large. Use '.' if you want a float literal.");
                        } else {
                            let mut v = s;
                            let mut e = ex;
                            while e > 0 {
                                v *= 10;
                                e -= 1;
                            }
                            return Literal(Integer(v));
                        }
                    }
                    // Should be unreachable:
                    (_, _) => invalid("Unparseable integer with exponent."),
                }
            }
            _ => match i64::from_str(self.slice()) {
                Ok(x) => Literal(Integer(x)),
                // Should be unreachable:
                Err(_) => invalid("Unparseable integer."),
            },
        }
    }

    fn eat_decimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.peek() {
                '0'..='9' => {
                    has_digits = true;
                    self.consume();
                }
                _ => break,
            }
        }
        has_digits
    }
}

fn invalid(msg: &'static str) -> TokenKind {
    Invalid(InvalidToken { msg })
}

fn is_whitespace(c: char) -> bool {
    matches!(
        c,
        // Sensible ASCII
        '\u{0009}'   // \t
        | '\u{000A}' // \n
        | '\u{000D}' // \r
        | '\u{0020}' // space
    )
}

fn is_id_start(c: char) -> bool {
    matches!(
        c,
        '_' | 'A'..='Z' | 'a'..='z'
    )
}

fn is_id_continue(c: char) -> bool {
    matches!(
        c,
        '_' | 'A'..='Z' | 'a'..='z' | '0'..='9'
    )
}

fn is_digit(c: char) -> bool {
    matches!(c, '0'..='9')
}

#[cfg(test)]
mod lexer_tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check_lexing(input: &str, expect: Expect) {
        let actual: String = RawLexer::new(input)
            .filter(|tok| match tok.kind {
                Whitespace => false,
                _ => true,
            })
            .map(|token| format!("{:?}\n", token))
            .collect();
        expect.assert_eq(&actual)
    }

    #[test]
    fn smoke_test() {
        check_lexing(
            "x + 2 + abc * def() / 456",
            expect![[r#"
                Ident("x") [len=1]
                BinOp(Plus) [len=1]
                Literal(Integer(2)) [len=1]
                BinOp(Plus) [len=1]
                Ident("abc") [len=3]
                BinOp(Star) [len=1]
                Ident("def") [len=3]
                OpenDelim(Parenthesis) [len=1]
                CloseDelim(Parenthesis) [len=1]
                BinOp(Slash) [len=1]
                Literal(Integer(456)) [len=3]
            "#]],
        );
    }

    #[test]
    fn literals() {
        check_lexing(
            "01234 12345 1e8 001e009",
            expect![[r#"
                Literal(Integer(1234)) [len=5]
                Literal(Integer(12345)) [len=5]
                Literal(Integer(100000000)) [len=3]
                Literal(Integer(1000000000)) [len=7]
            "#]],
        );
        check_lexing(
            "1.0 2.3e122 0.7e-93",
            expect![[r#"
            Literal(Float(1.0)) [len=3]
            Literal(Float(2.3e122)) [len=7]
            Literal(Float(7e-94)) [len=7]
        "#]],
        )
    }

    #[test]
    fn invalid_literals() {
        check_lexing(
            "1e-1",
            expect![[r#"
                Invalid(InvalidToken { msg: "Integer exponent cannot be negative. Use '.' if you want a float literal." }) [len=4]
            "#]],
        );
        check_lexing(
            "1e30",
            expect![[r#"
                Invalid(InvalidToken { msg: "Integer exponent is much too large. Use '.' if you want a float literal." }) [len=4]
            "#]],
        );
        check_lexing(
            "1.",
            expect![[r#"
            Invalid(InvalidToken { msg: "Need at least one digit after '.'." }) [len=2]
        "#]],
        );
        check_lexing(
            "1.e5",
            expect![[r#"
            Invalid(InvalidToken { msg: "Need at least one digit after '.'." }) [len=2]
            Ident("e5") [len=2]
        "#]],
        );
        check_lexing(
            "1.0e",
            expect![[r#"
            Invalid(InvalidToken { msg: "Need at least one digit after 'e'." }) [len=4]
        "#]],
        );
        check_lexing(
            "1.0e-",
            expect![[r#"
            Invalid(InvalidToken { msg: "Need at least one digit after 'e'." }) [len=5]
        "#]],
        );
        check_lexing(
            "@",
            expect![[r#"
            Invalid(InvalidToken { msg: "Unrecognized character." }) [len=1]
        "#]],
        );
    }

    #[test]
    fn symbols() {
        check_lexing(
            "+ - -> * / ( ) { } , : ;",
            expect![[r#"
                BinOp(Plus) [len=1]
                BinOp(Minus) [len=1]
                ThinArrow [len=2]
                BinOp(Star) [len=1]
                BinOp(Slash) [len=1]
                OpenDelim(Parenthesis) [len=1]
                CloseDelim(Parenthesis) [len=1]
                OpenDelim(CurlyBrace) [len=1]
                CloseDelim(CurlyBrace) [len=1]
                Comma [len=1]
                Colon [len=1]
                Semi [len=1]
            "#]],
        );
        check_lexing(
            "(-x)/{},:;->-+():1",
            expect![[r#"
                OpenDelim(Parenthesis) [len=1]
                BinOp(Minus) [len=1]
                Ident("x") [len=1]
                CloseDelim(Parenthesis) [len=1]
                BinOp(Slash) [len=1]
                OpenDelim(CurlyBrace) [len=1]
                CloseDelim(CurlyBrace) [len=1]
                Comma [len=1]
                Colon [len=1]
                Semi [len=1]
                ThinArrow [len=2]
                BinOp(Minus) [len=1]
                BinOp(Plus) [len=1]
                OpenDelim(Parenthesis) [len=1]
                CloseDelim(Parenthesis) [len=1]
                Colon [len=1]
                Literal(Integer(1)) [len=1]
            "#]],
        );
    }

    #[test]
    fn keywords() {
        check_lexing(
            "fn ret let if else",
            expect![[r#"
                KwFn [len=2]
                KwRet [len=3]
                KwLet [len=3]
                KwIf [len=2]
                KwElse [len=4]
            "#]],
        );
    }

    fn check_lexing_with_whitespace(input: &str, expect: Expect) {
        let actual: String = RawLexer::new(input)
            .map(|token| format!("{:?}\n", token))
            .collect();
        expect.assert_eq(&actual)
    }
    #[test]
    fn test_whitespace() {
        check_lexing_with_whitespace(
            "  \t\nx +\r\n  2\n\n",
            expect![[r#"
                Whitespace [len=4]
                Ident("x") [len=1]
                Whitespace [len=1]
                BinOp(Plus) [len=1]
                Whitespace [len=4]
                Literal(Integer(2)) [len=1]
                Whitespace [len=2]
            "#]],
        );
    }
}
