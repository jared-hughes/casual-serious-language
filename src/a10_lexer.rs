use crate::token::*;
use std::str::Chars;

use RawToken::*;
#[derive(Debug)]
pub enum RawToken<'a> {
    Useful(Token<'a>),
    Whitespace,
    Unknown(char),
}

/// Before any lexing, or after finishing a RawToken, `chars` points to the
/// first character that has not been in a token yet.
/// In the middle of finding the extent of a RawToken, `chars_token_start`
/// points to the first character that will be in this token.
pub struct Lexer<'a> {
    /// Iterator pointing at the current character
    chars: Chars<'a>,
    /// Iterator pointing at the character which starts this token
    chars_token_start: Chars<'a>,
}

const EOF_CHAR: char = '\0';

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        loop {
            let token = self.next_raw_token();
            return match token {
                // Stop the iterator.
                Useful(Eof) => None,
                // Common case
                Useful(tok) => Some(tok),
                // Try for the next token.
                Whitespace => continue,
                // Give up.
                Unknown(c) => panic!("Crazy character '{}'.", c),
            };
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
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

impl<'a> Lexer<'a> {
    fn slice(&self) -> &'a str {
        let start = self.chars_token_start.as_str();
        let num_bytes = start.len() - self.chars.as_str().len();
        return &start[0..num_bytes];
    }

    /// Parses a token from the input string, including whitespace.
    fn next_raw_token(&mut self) -> RawToken<'a> {
        let first_char = match self.consume() {
            Some(c) => c,
            None => return Useful(Eof),
        };
        let res = match first_char {
            c if is_whitespace(c) => {
                self.eat_while(is_whitespace);
                Whitespace
            }

            c if is_id_start(c) => {
                self.eat_while(is_id_continue);
                Useful(Ident(self.slice()))
            }

            // Numeric literal.
            '0'..='9' => {
                self.eat_while(is_digit);
                let mut val: u32 = 0;
                for c in self.slice().chars() {
                    val *= 10;
                    val += c.to_digit(10).expect("Digits are digits");
                }
                Useful(Literal(Integer(val)))
            }

            '+' => Useful(BinOp(Plus)),
            '-' => Useful(BinOp(Minus)),
            '*' => Useful(BinOp(Star)),
            '/' => Useful(BinOp(Slash)),
            '(' => Useful(OpenDelim(Parenthesis)),
            ')' => Useful(CloseDelim(Parenthesis)),

            c => Unknown(c),
        };
        self.chars_token_start = self.chars.clone();
        res
    }
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
        let actual: String = Lexer::new(input)
            .map(|token| format!("{:?}\n", token))
            .collect();
        expect.assert_eq(&actual)
    }

    #[test]
    fn smoke_test() {
        check_lexing(
            "x + 2 + abc * def() / 456",
            expect![[r#"
                Ident("x")
                BinOp(Plus)
                Literal(Integer(2))
                BinOp(Plus)
                Ident("abc")
                BinOp(Star)
                Ident("def")
                OpenDelim(Parenthesis)
                CloseDelim(Parenthesis)
                BinOp(Slash)
                Literal(Integer(456))
            "#]],
        );
    }
}
