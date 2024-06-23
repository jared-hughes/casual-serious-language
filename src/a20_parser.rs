use crate::ast;
use crate::errors::{Diag, Diagnostic};
use crate::lexer::Lexer;
use crate::parser_err::ParseErr;
use crate::token::*;
use std::iter::Peekable;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum BindingPower {
    Top,
    Add,
    Mul,
}

struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

type Bexpr = Box<ast::Expr>;
type ExprResult = Result<Bexpr, Diag>;

pub fn parse(input: &str) -> ExprResult {
    Parser::new(input).parse()
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Parser<'a> {
        Parser {
            lexer: Lexer::new(input).peekable(),
        }
    }

    pub fn parse(&mut self) -> ExprResult {
        let expr = self.parse_main(BindingPower::Top)?;
        if let Some(_) = self.lexer.peek() {
            return Err(ParseErr::ExpectedConsequent.into_diag());
        }
        Ok(expr)
    }

    fn next(&mut self) -> Token<'a> {
        self.lexer.next().unwrap_or(Eof)
    }

    fn peek(&mut self) -> Token<'a> {
        *self.lexer.peek().unwrap_or(&Eof)
    }
}

impl<'a> Parser<'a> {
    fn parse_main(&mut self, last_bp: BindingPower) -> ExprResult {
        let mut left = self.parse_initial()?;
        while let Some(_) = self.lexer.peek() {
            if let Some(tok) = self.consequent_good(last_bp)? {
                left = self.parse_consequent(left, tok)?;
            } else {
                break;
            };
        }
        Ok(left)
    }

    fn parse_initial(&mut self) -> ExprResult {
        match self.next() {
            Literal(x) => Ok(Box::new(ast::Literal(x))),
            Ident(s) => Ok(Box::new(ast::Ident(s.to_owned()))),
            OpenDelim(Parenthesis) => {
                let inner = self.parse_main(BindingPower::Top)?;
                let close_paren = self.next();
                if let CloseDelim(Parenthesis) = close_paren {
                    return Ok(inner);
                } else {
                    return Err(ParseErr::OpenParenMissingCloseParen.into_diag());
                }
            }
            BinOp(Plus) | BinOp(Minus) => Err(ParseErr::NoUnaryPlusMinus.into_diag()),
            BinOp(_) => Err(ParseErr::UnexpectedBinaryInitial.into_diag()),
            CloseDelim(Parenthesis) => Err(ParseErr::UnmatchedCloseParen.into_diag()),
            Eof => Err(ParseErr::UnexpectedEOF.into_diag()),
        }
    }

    /// None = invalid consequent, or is lower binding power than last_bp.
    fn consequent_good(&mut self, last_bp: BindingPower) -> Result<Option<Token<'a>>, Diag> {
        Ok(match self.peek() {
            BinOp(t) => {
                if binop_power(t) <= last_bp {
                    None
                } else {
                    Some(self.next())
                }
            }
            _ => None,
        })
    }

    fn parse_consequent(&mut self, left: Bexpr, tok: Token) -> ExprResult {
        match tok {
            BinOp(t) => Ok(self.parse_binop(left, t)?),
            // Expected consequent_good to return `None` first, if this is reached.
            _ => panic!("Invariant violation: entered non-good consequent"),
        }
    }

    fn parse_binop(&mut self, left: Bexpr, tok: BinOpToken) -> ExprResult {
        let right = self.parse_main(binop_power(tok))?;
        return Ok(Box::new(ast::Binary(translate_binop(tok), left, right)));
    }
}

fn binop_power(t: BinOpToken) -> BindingPower {
    match t {
        Plus | Minus => BindingPower::Add,
        Star | Slash => BindingPower::Mul,
    }
}

fn translate_binop(t: BinOpToken) -> ast::BinOp {
    match t {
        Plus => ast::Add,
        Minus => ast::Sub,
        Star => ast::Mul,
        Slash => ast::Div,
    }
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check_parsing(input: &str, expect: Expect) {
        let actual: String = match parse(input) {
            Ok(expr) => format!("{:#?}", expr),
            Err(diag) => format!("{:#?}", diag),
        };
        expect.assert_eq(&actual)
    }

    #[test]
    fn atoms() {
        check_parsing("x", expect![[r#"Ident("x")"#]]);
        check_parsing("1", expect!["Literal(Integer(1))"]);
    }

    #[test]
    fn binary() {
        check_parsing(
            "1+2",
            expect![[r#"
                Binary[Add](
                    Literal(Integer(1)),
                    Literal(Integer(2)),
                )"#]],
        );
        check_parsing(
            "1-2",
            expect![[r#"
                Binary[Sub](
                    Literal(Integer(1)),
                    Literal(Integer(2)),
                )"#]],
        );
        check_parsing(
            "1*2",
            expect![[r#"
                Binary[Mul](
                    Literal(Integer(1)),
                    Literal(Integer(2)),
                )"#]],
        );
        check_parsing(
            "1/2",
            expect![[r#"
                Binary[Div](
                    Literal(Integer(1)),
                    Literal(Integer(2)),
                )"#]],
        );
    }

    #[test]
    fn smoke_test() {
        check_parsing(
            "x + (2 + abc) * def / 456",
            expect![[r#"
                Binary[Add](
                    Ident("x"),
                    Binary[Div](
                        Binary[Mul](
                            Binary[Add](
                                Literal(Integer(2)),
                                Ident("abc"),
                            ),
                            Ident("def"),
                        ),
                        Literal(Integer(456)),
                    ),
                )"#]],
        )
    }

    #[test]
    fn error_token_in_initial() {
        check_parsing("", expect!["Hold your horses. An EOF already?"]);
        check_parsing(
            "/",
            expect!["Unexpected binary operator in initial position."],
        );
        check_parsing("+", expect!["Unary '+' or '-' is not supported yet."]);
        check_parsing("x+", expect!["Hold your horses. An EOF already?"]);
        check_parsing(
            ")",
            expect!["What's this ')' doing here? I don't see a '('"],
        );
    }

    #[test]
    fn error_token_in_consequent() {
        check_parsing(
            "x x",
            expect!["Unexpected token here. A binary operator like + may be preferred."],
        );
        check_parsing(
            "x)",
            expect!["Unexpected token here. A binary operator like + may be preferred."],
        );
    }

    #[test]
    fn error_token_in_special() {
        check_parsing("(x++", expect!["Unary '+' or '-' is not supported yet."]);
    }
}
