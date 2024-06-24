use crate::ast;
use crate::errors::{Diag, Diagnostic};
use crate::lexer::Lexer;
use crate::parser_err::ParseErr;
use crate::span::{respan, span, Span};
use crate::token::*;
use std::iter::Peekable;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum BindingPower {
    // Inside parens
    Top,
    // +, -
    Add,
    // *, /
    Mul,
    // +, - but prefix
    Prefix,
}

struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

type Bexpr = Box<ast::Expr>;
type ExprResult = Result<Bexpr, Diag>;

fn expr(body: ast::ExprInner, span: Span) -> Bexpr {
    Box::new(ast::Expr::new(body, span))
}

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
        let tok = self.peek();
        match tok.kind {
            Eof => Ok(expr),
            _ => Err(ParseErr::ExpectedConsequent(tok.span).into_diag()),
        }
    }

    fn next(&mut self) -> Token<'a> {
        self.lexer
            .next()
            .expect("Lexer should emit Eof tokens, not None.")
    }

    fn peek(&mut self) -> Token<'a> {
        *self
            .lexer
            .peek()
            .expect("Lexer should emit Eof tokens, not None.")
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
        let start = self.next();
        match start.kind {
            Literal(x) => Ok(expr(ast::Literal(x), start.span)),
            Ident(s) => Ok(expr(ast::Ident(s.to_owned()), start.span)),
            OpenDelim(Parenthesis) => {
                let inner = self.parse_main(BindingPower::Top)?;
                let close_paren = self.next();
                if let CloseDelim(Parenthesis) = close_paren.kind {
                    return Ok(inner);
                } else {
                    return Err(ParseErr::OpenParenMissingCloseParen(close_paren.span).into_diag());
                }
            }
            BinOp(Plus) => {
                return Err(ParseErr::UnaryPlusDisallowed(start.span).into_diag());
            }
            BinOp(Minus) => {
                let inner = self.parse_main(BindingPower::Prefix)?;
                let unop = respan(ast::UnaryOpKind::Neg, start.span);
                let s = span(start.span.lo, inner.span.hi);
                Ok(expr(ast::Unary(unop, inner), s))
            }
            BinOp(_) => Err(ParseErr::UnexpectedBinaryInitial(start.span).into_diag()),
            CloseDelim(Parenthesis) => Err(ParseErr::UnmatchedCloseParen(start.span).into_diag()),
            Eof => Err(ParseErr::UnexpectedEOF(start.span).into_diag()),
            Whitespace => panic!("Whitespace should be skipped"),
            Invalid(x) => Err(ParseErr::InvalidToken(start.span, x).into_diag()),
        }
    }

    /// None = invalid consequent, or is lower binding power than last_bp.
    fn consequent_good(&mut self, last_bp: BindingPower) -> Result<Option<Token<'a>>, Diag> {
        Ok(match self.peek().kind {
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
        match tok.kind {
            BinOp(t) => Ok(self.parse_binop(left, t, tok.span)?),
            // Expected consequent_good to return `None` first, if this is reached.
            _ => panic!("Invariant violation: entered non-good consequent"),
        }
    }

    fn parse_binop(&mut self, left: Bexpr, tok: BinOpToken, sp: Span) -> ExprResult {
        let right = self.parse_main(binop_power(tok))?;
        let binop = respan(translate_binop(tok), sp);
        let s = span(left.span.lo, right.span.hi);
        return Ok(expr(ast::Binary(binop, left, right), s));
    }
}

fn binop_power(t: BinOpToken) -> BindingPower {
    match t {
        Plus | Minus => BindingPower::Add,
        Star | Slash => BindingPower::Mul,
    }
}

fn translate_binop(t: BinOpToken) -> ast::BinOpKind {
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
        check_parsing("x", expect![[r#"(1)Ident("x")"#]]);
        check_parsing("1", expect!["(1)Literal(Integer(1))"]);
    }

    #[test]
    fn binary() {
        check_parsing(
            "1+2",
            expect![[r#"
                (1-3)Binary[Add(2)](
                    (1)Literal(Integer(1)),
                    (3)Literal(Integer(2)),
                )"#]],
        );
        check_parsing(
            "1-2",
            expect![[r#"
                (1-3)Binary[Sub(2)](
                    (1)Literal(Integer(1)),
                    (3)Literal(Integer(2)),
                )"#]],
        );
        check_parsing(
            "1*2",
            expect![[r#"
                (1-3)Binary[Mul(2)](
                    (1)Literal(Integer(1)),
                    (3)Literal(Integer(2)),
                )"#]],
        );
        check_parsing(
            "1/2",
            expect![[r#"
                (1-3)Binary[Div(2)](
                    (1)Literal(Integer(1)),
                    (3)Literal(Integer(2)),
                )"#]],
        );
    }

    #[test]
    fn unary() {
        check_parsing("+x", expect!["At 1: Leading '+' is not supported."]);
        check_parsing(
            "-x",
            expect![[r#"
                (1-2)Unary[Neg(1)](
                    (2)Ident("x"),
                )"#]],
        );
        check_parsing(
            "-x+3",
            expect![[r#"
                (1-4)Binary[Add(3)](
                    (1-2)Unary[Neg(1)](
                        (2)Ident("x"),
                    ),
                    (4)Literal(Integer(3)),
                )"#]],
        );
        check_parsing(
            "-x*3",
            expect![[r#"
                (1-4)Binary[Mul(3)](
                    (1-2)Unary[Neg(1)](
                        (2)Ident("x"),
                    ),
                    (4)Literal(Integer(3)),
                )"#]],
        );
        check_parsing(
            "3*-x",
            expect![[r#"
                (1-4)Binary[Mul(2)](
                    (1)Literal(Integer(3)),
                    (3-4)Unary[Neg(3)](
                        (4)Ident("x"),
                    ),
                )"#]],
        );
        check_parsing(
            "3--x",
            expect![[r#"
                (1-4)Binary[Sub(2)](
                    (1)Literal(Integer(3)),
                    (3-4)Unary[Neg(3)](
                        (4)Ident("x"),
                    ),
                )"#]],
        );
    }

    #[test]
    fn smoke_test() {
        check_parsing(
            "x + (2 + abc) * def / 456",
            expect![[r#"
                (1-25)Binary[Add(3)](
                    (1)Ident("x"),
                    (6-25)Binary[Div(21)](
                        (6-19)Binary[Mul(15)](
                            (6-12)Binary[Add(8)](
                                (6)Literal(Integer(2)),
                                (10-12)Ident("abc"),
                            ),
                            (17-19)Ident("def"),
                        ),
                        (23-25)Literal(Integer(456)),
                    ),
                )"#]],
        )
    }

    #[test]
    fn error_token_in_initial() {
        check_parsing("", expect!["At (!1,1!): Hold your horses. An EOF already?"]);
        check_parsing(
            "/",
            expect!["At 1: Unexpected binary operator in initial position."],
        );
        check_parsing("+", expect!["At 1: Leading '+' is not supported."]);
        check_parsing(
            "-",
            expect!["At (!2,2!): Hold your horses. An EOF already?"],
        );
        check_parsing(
            "x+",
            expect!["At (!3,3!): Hold your horses. An EOF already?"],
        );
        check_parsing(
            ")",
            expect!["At 1: What's this ')' doing here? I don't see a '('"],
        );
        check_parsing("1.e", expect!["At 1-2: Need at least one digit after '.'."]);
    }

    #[test]
    fn error_token_in_consequent() {
        check_parsing(
            "x x",
            expect!["At 3: Unexpected token here. A binary operator like + may be preferred."],
        );
        check_parsing(
            "x)",
            expect!["At 2: Unexpected token here. A binary operator like + may be preferred."],
        );
        check_parsing(
            "x 1.e",
            expect!["At 3-4: Unexpected token here. A binary operator like + may be preferred."],
        );
    }

    #[test]
    fn error_token_in_special() {
        check_parsing("(x++", expect!["At 4: Leading '+' is not supported."]);
    }
}
