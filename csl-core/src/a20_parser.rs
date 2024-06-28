use crate::ast;
use crate::errors::Diag;
use crate::lexer::Lexer;
use crate::parser_err::ParseErrKind as PE;
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
    // f()
    Call,
}

struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

type Bexpr = Box<ast::Expr>;
type ExprResult = Result<Bexpr, Diag>;
type ProgramResult = Result<ast::Program, Diag>;

fn expr(body: ast::ExprInner, span: Span) -> Bexpr {
    Box::new(ast::Expr::new(body, span))
}

pub fn parse(input: &str) -> ProgramResult {
    Parser::new(input).parse()
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Parser<'a> {
        Parser {
            lexer: Lexer::new(input).peekable(),
        }
    }

    pub fn parse(&mut self) -> ProgramResult {
        let prog = self.parse_program()?;
        self.assert_stream_done()?;
        Ok(prog)
    }

    pub fn assert_stream_done(&mut self) -> Result<(), Diag> {
        let tok = self.peek()?;
        let Eof = tok.kind else {
            Err(PE::ExpectedConsequent.span(tok.span))?
        };
        Ok(())
    }

    fn next(&mut self) -> Result<Token<'a>, Diag> {
        let next = self
            .lexer
            .next()
            .expect("Lexer should emit Eof tokens, not None.");
        preprocess(next)
    }

    fn peek(&mut self) -> Result<Token<'a>, Diag> {
        let peek = *self
            .lexer
            .peek()
            .expect("Lexer should emit Eof tokens, not None.");
        preprocess(peek)
    }
}

fn preprocess<'a>(tok: Token<'a>) -> Result<Token<'a>, Diag> {
    match tok.kind {
        Invalid(x) => Err(PE::InvalidToken(x).span(tok.span)),
        _ => Ok(tok),
    }
}

macro_rules! consume_token {
    ($self:ident, $tok:pat, $err:expr) => {{
        let next = $self.next()?;
        let $tok = next.kind else {
            return Err($err.span(next.span));
        };
        next
    }};
}

macro_rules! consume_ident {
    ($self:ident, $err:expr) => {{
        let next = $self.next()?;
        let Ident(name) = next.kind else {
            return Err($err.span(next.span));
        };
        ast::Ident {
            name: name.to_owned(),
            span: next.span,
        }
    }};
}

fn needs_semi(ex: &ast::ExprInner) -> bool {
    match ex {
        ast::ExprInner::FnDefinition(_) => false,
        _ => true,
    }
}

/* Statement-level parser */
impl<'a> Parser<'a> {
    fn parse_program(&mut self) -> ProgramResult {
        let body = self.parse_stmts()?;
        return Ok(ast::Program { body });
    }

    /// Parse statements until a '}' or EOF.
    fn parse_stmts(&mut self) -> Result<Vec<ast::Expr>, Diag> {
        let mut stmts = vec![];
        loop {
            self.skip_semis()?;
            // Stop if there's a `}` or Eof
            if terminates_block(self.peek()?.kind) {
                return Ok(stmts);
            }
            // Parse the main expr.
            let expr = self.parse_main(BindingPower::Top)?;
            // ;
            if needs_semi(&expr.body) {
                consume_token!(self, Semi, PE::ExpectedSemi);
            }
            stmts.push(*expr);
        }
    }

    fn skip_semis(&mut self) -> Result<(), Diag> {
        let mut next = self.peek()?;
        while let Semi = next.kind {
            self.next()?;
            next = self.peek()?;
        }
        Ok(())
    }
}

/* Expression parser */
impl<'a> Parser<'a> {
    #[cfg(test)]
    pub fn parse_expr_for_tests(&mut self) -> ExprResult {
        let expr = self.parse_main(BindingPower::Top)?;
        self.assert_stream_done()?;
        Ok(expr)
    }

    /// Parse an expression
    pub fn parse_main(&mut self, last_bp: BindingPower) -> ExprResult {
        let mut left = self.parse_initial()?;
        loop {
            if let Eof = self.peek()?.kind {
                break;
            }
            if let Some(tok) = self.consequent_good(last_bp)? {
                left = self.parse_consequent(left, tok)?;
            } else {
                break;
            };
        }
        Ok(left)
    }

    fn parse_initial(&mut self) -> ExprResult {
        let start = self.next()?;
        match start.kind {
            Literal(x) => Ok(expr(ast::Literal(x), start.span)),
            Ident(s) => Ok(expr(ast::Ident(s.to_owned()), start.span)),
            OpenDelim(Parenthesis) => {
                let inner = self.parse_main(BindingPower::Top)?;
                let close_paren = consume_token!(
                    self,
                    CloseDelim(Parenthesis),
                    PE::OpenParenMissingCloseParen
                );
                let s = span(start.span.lo, close_paren.span.hi);
                Ok(expr(ast::Paren(inner), s))
            }
            KwRet => {
                let inner = self.parse_main(BindingPower::Top)?;
                let s = span(start.span.lo, inner.span.hi);
                Ok(expr(ast::Ret(start.span, inner), s))
            }
            BinOp(Plus) => {
                return Err(PE::UnaryPlusDisallowed.span(start.span));
            }
            BinOp(Minus) => {
                let inner = self.parse_main(BindingPower::Prefix)?;
                let unop = respan(ast::UnaryOpKind::Neg, start.span);
                let s = span(start.span.lo, inner.span.hi);
                Ok(expr(ast::Unary(unop, inner), s))
            }
            BinOp(_) => Err(PE::UnexpectedBinaryInitial.span(start.span)),
            CloseDelim(Parenthesis) => Err(PE::UnmatchedCloseParen.span(start.span)),
            Eof => Err(PE::UnexpectedEOF.span(start.span)),
            Whitespace | Invalid(_) => panic!("Whitespace should be skipped"),
            OpenDelim(CurlyBrace) | CloseDelim(CurlyBrace) | Colon | Comma | ThinArrow | Semi => {
                Err(PE::GeneralUnexpected.span(start.span))
            }
            KwFn => self.parse_fn(start),
        }
    }

    /// None = invalid consequent, or is lower binding power than last_bp.
    fn consequent_good(&mut self, last_bp: BindingPower) -> Result<Option<Token<'a>>, Diag> {
        Ok(match self.peek()?.kind {
            BinOp(t) => {
                if binop_power(t) <= last_bp {
                    None
                } else {
                    Some(self.next()?)
                }
            }
            OpenDelim(Parenthesis) => {
                if BindingPower::Call <= last_bp {
                    None
                } else {
                    Some(self.next()?)
                }
            }
            _ => None,
        })
    }

    fn parse_consequent(&mut self, left: Bexpr, tok: Token) -> ExprResult {
        match tok.kind {
            BinOp(t) => Ok(self.parse_binop(left, t, tok.span)?),
            OpenDelim(Parenthesis) => Ok(self.parse_call(left)?),
            // Expected consequent_good to return `None` first, if this is reached.
            _ => panic!("Invariant violation: entered non-good consequent"),
        }
    }

    fn parse_binop(&mut self, left: Bexpr, tok: BinOpToken, sp: Span) -> ExprResult {
        // All operations are left-assocative.
        // Subtract one from the binding power here if you want right-associative.
        let right = self.parse_main(binop_power(tok))?;
        let binop = respan(translate_binop(tok), sp);
        let s = span(left.span.lo, right.span.hi);
        return Ok(expr(ast::Binary(binop, left, right), s));
    }

    fn parse_call(&mut self, left: Bexpr) -> ExprResult {
        let mut args = vec![];
        // disallow f(,)
        let after_open_paren = self.peek()?;
        if let Comma = after_open_paren.kind {
            return Err(PE::FnBadComma.span(after_open_paren.span));
        }
        // main loop
        while !terminates_fn_params(self.peek()?.kind) {
            let arg = self.parse_main(BindingPower::Top)?;
            // , or )
            let after_arg = self.peek()?;
            // Don't need comma ',' if next token is ')'
            if !terminates_fn_params(after_arg.kind) {
                // Trailing comma, or comma between args
                consume_token!(self, Comma, PE::CallExpComma);
            }
            args.push(*arg);
        }
        let close_paren = consume_token!(self, CloseDelim(Parenthesis), PE::CallExpCloseParen);
        let s = span(left.span.lo, close_paren.span.hi);
        return Ok(expr(ast::FnCall(left, args), s));
    }

    fn parse_fn(&mut self, fn_token: Token) -> ExprResult {
        let fn_name = consume_ident!(self, PE::FnExpectedName);
        // (
        consume_token!(self, OpenDelim(Parenthesis), PE::FnExpOpenParen(fn_name));
        let mut params: Vec<ast::FunctionParam> = vec![];
        // disallow f(,)
        let after_open_paren = self.peek()?;
        if let Comma = after_open_paren.kind {
            return Err(PE::FnBadComma.span(after_open_paren.span));
        }
        // main loop
        while !terminates_fn_params(self.peek()?.kind) {
            // x
            let param = consume_ident!(self, PE::FnExpParameter(fn_name));
            // :
            consume_token!(self, Colon, PE::FnExpColon(fn_name, param));
            // i64
            let type_name = consume_ident!(self, PE::FnExpParamType(fn_name, param));
            // , or )
            let after_param = self.peek()?;
            // Don't need comma ',' if next token is ')'
            if !terminates_fn_params(after_param.kind) {
                // Trailing comma, or comma between args
                consume_token!(self, Comma, PE::CallExpComma);
            }
            params.push(ast::FunctionParam {
                name: param,
                param_type: type_name,
            });
        }
        // )
        consume_token!(self, CloseDelim(Parenthesis), PE::FnExpCloseParen(fn_name));
        // ->
        consume_token!(self, ThinArrow, PE::FnExpThinArrow(fn_name));
        // i64
        let return_type = consume_ident!(self, PE::FnExpReturnType(fn_name));
        // {
        consume_token!(self, OpenDelim(CurlyBrace), PE::FnExpOpenCurly(fn_name));
        // ret x * 2;
        let body = self.parse_stmts()?;
        // }
        let close_curly =
            consume_token!(self, CloseDelim(CurlyBrace), PE::FnExpCloseCurly(fn_name));
        return Ok(expr(
            ast::FnDefinition(ast::FunctionDefinition {
                fn_name,
                params,
                body,
                return_type,
            }),
            span(fn_token.span.lo, close_curly.span.hi),
        ));
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

/// This function must return true for `CloseDelim(Parenthesis)` and `Eof`, and
/// false for `Semi` and anything that could start a valid statement.
/// The behavior otherwise is arbitrary. If this returns true, we'll get
/// "Expected }", and if this returns false, we'll get some error about invalid initial.
fn terminates_block(tok: TokenKind) -> bool {
    match tok {
        CloseDelim(_) | Eof | Invalid(_) => true,
        Semi | BinOp(_) | OpenDelim(_) | ThinArrow | Comma | Literal(_) | Colon | Ident(_)
        | KwFn | KwRet => false,
        Whitespace => panic!("Whitespace should be skipped"),
    }
}

/// This function must return true for `CloseDelim(Parenthesis)` and `Eof`, and
/// false for `Comma`. The behavior otherwise is arbitrary.
/// If this returns true on a peeked token, we want that token to say
/// "Expected )" instead of "Expected ','" in an error message.
fn terminates_fn_params(tok: TokenKind) -> bool {
    match tok {
        BinOp(_) | OpenDelim(_) | CloseDelim(_) | Eof | ThinArrow | Invalid(_) | Semi => true,
        Comma | Literal(_) | Colon | Ident(_) | KwFn | KwRet => false,
        Whitespace => panic!("Whitespace should be skipped"),
    }
}

#[cfg(test)]
mod parser_expr_tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check_parsing(input: &str, expect: Expect) {
        let actual: String = match Parser::new(input).parse_expr_for_tests() {
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
                    (5-25)Binary[Div(21)](
                        (5-19)Binary[Mul(15)](
                            (5-13)paren@(6-12)Binary[Add(8)](
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
            expect!["At 3-4: Need at least one digit after '.'."],
        );
    }

    #[test]
    fn error_token_in_special() {
        check_parsing("(x++", expect!["At 4: Leading '+' is not supported."]);
    }

    #[test]
    fn fn_call() {
        check_parsing("f()", expect![[r#"(1-3)call((1)Ident("f"))"#]]);
        check_parsing(
            "f(x)",
            expect![[r#"
                (1-4)call((1)Ident("f"))(
                    (3)Ident("x"),
                )"#]],
        );
        check_parsing(
            "f(x,y)",
            expect![[r#"
                (1-6)call((1)Ident("f"))(
                    (3)Ident("x"),
                    (5)Ident("y"),
                )"#]],
        );
        check_parsing(
            "f(x,)",
            expect![[r#"
                (1-5)call((1)Ident("f"))(
                    (3)Ident("x"),
                )"#]],
        );
        check_parsing(
            "f(x,y,)",
            expect![[r#"
            (1-7)call((1)Ident("f"))(
                (3)Ident("x"),
                (5)Ident("y"),
            )"#]],
        );
    }

    #[test]
    fn error_call() {
        check_parsing(
            "f(",
            expect!["At (!3,3!): Expected ')' to end function arguments."],
        );
        check_parsing(
            "f(,)",
            expect!["At 3: Comma ',' is not allowed before first argument."],
        );
        check_parsing(
            "f(,x)",
            expect!["At 3: Comma ',' is not allowed before first argument."],
        );
        check_parsing(
            "f(x:)",
            expect!["At 4: Expected ',' after function argument."],
        );
        check_parsing(
            "f(x ;",
            expect!["At 5: Expected ')' to end function arguments."],
        );
    }
}

#[cfg(test)]
mod parser_stmt_tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check_parsing(input: &str, expect: Expect) {
        let actual: String = match parse(input) {
            Ok(prog) => format!("{:#?}", prog),
            Err(diag) => format!("{:#?}", diag),
        };
        expect.assert_eq(&actual)
    }

    #[test]
    fn multi_stmts() {
        check_parsing(
            "1+2;\
            fn three() -> i64 { ret 3; }
            fn four() -> i64 { ret 4; }
            4+6;",
            expect![[r#"
                (1-3)Binary[Add(2)](
                    (1)Literal(Integer(1)),
                    (3)Literal(Integer(2)),
                )
                (5-32)FnDefinition[three]() {
                    (25-29)ret(25-27) (
                        (29)Literal(Integer(3)),
                    ),
                }
                (46-72)FnDefinition[four]() {
                    (65-69)ret(65-67) (
                        (69)Literal(Integer(4)),
                    ),
                }
                (86-88)Binary[Add(87)](
                    (86)Literal(Integer(4)),
                    (88)Literal(Integer(6)),
                )
            "#]],
        );
    }

    #[test]
    fn fn_params() {
        check_parsing(
            "fn three() -> i64 { ret 3; }",
            expect![[r#"
                (1-28)FnDefinition[three]() {
                    (21-25)ret(21-23) (
                        (25)Literal(Integer(3)),
                    ),
                }
            "#]],
        );
        check_parsing(
            "fn double(x: i64) -> i64 { ret 2*x; }",
            expect![[r#"
                (1-37)FnDefinition[double](x: i64) {
                    (28-34)ret(28-30) (
                        (32-34)Binary[Mul(33)](
                            (32)Literal(Integer(2)),
                            (34)Ident("x"),
                        ),
                    ),
                }
            "#]],
        );
        check_parsing(
            "fn add(x: i64, y: i64) -> i64 { ret x + y; }",
            expect![[r#"
                (1-44)FnDefinition[add](x: i64, y: i64) {
                    (33-41)ret(33-35) (
                        (37-41)Binary[Add(39)](
                            (37)Ident("x"),
                            (41)Ident("y"),
                        ),
                    ),
                }
            "#]],
        );
    }

    #[test]
    fn fn_trailing_comma() {
        check_parsing(
            "fn double(x: i64,) -> i64 { ret 2*x; }",
            expect![[r#"
                (1-38)FnDefinition[double](x: i64) {
                    (29-35)ret(29-31) (
                        (33-35)Binary[Mul(34)](
                            (33)Literal(Integer(2)),
                            (35)Ident("x"),
                        ),
                    ),
                }
            "#]],
        );
        check_parsing(
            "fn add(x: i64, y: i64,) -> i64 { ret x + y; }",
            expect![[r#"
                (1-45)FnDefinition[add](x: i64, y: i64) {
                    (34-42)ret(34-36) (
                        (38-42)Binary[Add(40)](
                            (38)Ident("x"),
                            (42)Ident("y"),
                        ),
                    ),
                }
            "#]],
        );
    }

    #[test]
    fn fn_body() {
        check_parsing(
            "fn f() -> i64 { }",
            expect![[r#"
                (1-17)FnDefinition[f]() {}
            "#]],
        );
        check_parsing(
            "fn f() -> i64 { f(f(f(f()))); }",
            expect![[r#"
                (1-31)FnDefinition[f]() {
                    (17-28)call((17)Ident("f"))(
                        (19-27)call((19)Ident("f"))(
                            (21-26)call((21)Ident("f"))(
                                (23-25)call((23)Ident("f")),
                            ),
                        ),
                    ),
                }
            "#]],
        );
        check_parsing(
            "fn f() -> i64 { ;;1+2;;3+4;5+6;;; }",
            expect![[r#"
                (1-35)FnDefinition[f]() {
                    (19-21)Binary[Add(20)](
                        (19)Literal(Integer(1)),
                        (21)Literal(Integer(2)),
                    ),
                    (24-26)Binary[Add(25)](
                        (24)Literal(Integer(3)),
                        (26)Literal(Integer(4)),
                    ),
                    (28-30)Binary[Add(29)](
                        (28)Literal(Integer(5)),
                        (30)Literal(Integer(6)),
                    ),
                }
            "#]],
        );
    }

    #[test]
    fn fn_error_cases() {
        check_parsing("fn {}", expect!["At 4: Expected identifier. Functions must have a name. For example: `fn add(x: u64, y: u64) -> u64 { x + y }`"]);
        check_parsing(
            "fn double{}",
            expect!["At 10: Expected '(' to begin parameter declaration for function 'double'. For example: `fn add(x: u64, y: u64) -> u64 { x + y }`"],
        );
        check_parsing(
            "fn three(,) -> i64 { ret 3; }",
            expect!["At 10: Comma ',' is not allowed before first argument."],
        );
        check_parsing(
            "fn add(x i64)",
            expect!["At 10-12: Expected ':' to provide the type of the parameter 'x' for function 'add'. For example: `fn add(x: u64, y: u64) -> u64 { x + y }`"],
        );
        check_parsing(
            "fn add(x, y: i64)",
            expect!["At 9: Expected ':' to provide the type of the parameter 'x' for function 'add'. For example: `fn add(x: u64, y: u64) -> u64 { x + y }`"],
        );
        check_parsing(
            "fn add(x:, y: i64)",
            expect!["At 10: Expected an identifier to provide the type of the parameter 'x' for function 'add'. For example: `fn add(x: u64, y: u64) -> u64 { x + y }`"],
        );
        check_parsing(
            "fn add(x: i64; y: i64)",
            expect!["At 14: Expected ')' to end function parameters for function 'add'. For example: `fn add(x: u64, y: u64) -> u64 { x + y }`"],
        );
        check_parsing(
            "fn double(x: i64 { ret 2 * x; }",
            expect!["At 18: Expected ')' to end function parameters for function 'double'. For example: `fn add(x: u64, y: u64) -> u64 { x + y }`"],
        );
        check_parsing(
            "fn double(x: i64 -> { ret 2 * x; }",
            expect!["At 18-19: Expected ')' to end function parameters for function 'double'. For example: `fn add(x: u64, y: u64) -> u64 { x + y }`"],
        );
        check_parsing(
            "fn f() -> i64 {",
            expect!["At (!16,16!): Expected '}' to end the body of function 'f'. For example: `fn add(x: u64, y: u64) -> u64 { x + y }`"],
        );
        check_parsing(
            "fn f() -> i64 { ) }",
            expect!["At 17: Expected '}' to end the body of function 'f'. For example: `fn add(x: u64, y: u64) -> u64 { x + y }`"],
        );
    }
}
