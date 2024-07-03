use crate::ast::{self, Mutability};
use crate::errors::{Diag, Diagnostic};
use crate::lexer::Lexer;
use crate::parser_err as PE;
use crate::span::{respan, span, Span};
use crate::token::*;
use std::iter::Peekable;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum BindingPower {
    // Inside parens, to the right of `=`, etc.
    Top,
    // ||
    Or,
    // &&
    And,
    // <, <=, ==, !=, >=, >
    Compare,
    // +, -
    Add,
    // *, /
    Mul,
    // !, prefix -
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

pub(crate) fn parse(input: &str) -> ProgramResult {
    Parser::new(input).parse()
}

fn err<T>(x: impl Diagnostic) -> Result<T, Diag> {
    Err(x.into_diag())
}

impl<'a> Parser<'a> {
    pub(crate) fn new(input: &'a str) -> Parser<'a> {
        Parser {
            lexer: Lexer::new(input).peekable(),
        }
    }

    pub(crate) fn parse(&mut self) -> ProgramResult {
        let prog = self.parse_program()?;
        self.assert_stream_done()?;
        Ok(prog)
    }

    pub(crate) fn assert_stream_done(&mut self) -> Result<(), Diag> {
        let tok = self.peek()?;
        let Eof = tok.kind else {
            err(PE::ExpectedConsequent.span(tok.span))?
        };
        Ok(())
    }

    /// Only fails for Invalid tokens
    fn next(&mut self) -> Result<Token<'a>, Diag> {
        let next = self
            .lexer
            .next()
            .expect("Lexer should emit Eof tokens, not None.");
        preprocess(next)
    }

    /// Only fails for Invalid tokens
    fn peek(&mut self) -> Result<Token<'a>, Diag> {
        let peek = *self
            .lexer
            .peek()
            .expect("Lexer should emit Eof tokens, not None.");
        preprocess(peek)
    }
}

fn preprocess(tok: Token<'_>) -> Result<Token<'_>, Diag> {
    match tok.kind {
        Invalid(x) => err(PE::InvalidToken { token: x }.span(tok.span)),
        _ => Ok(tok),
    }
}

macro_rules! consume_token {
    ($self:ident, $tok:pat, $err:expr) => {{
        let next = $self.next()?;
        let $tok = next.kind else {
            return err($err.span(next.span));
        };
        next
    }};
}

macro_rules! consume_ident {
    ($self:ident, $err:expr) => {{
        let next = $self.next()?;
        let Ident(name) = next.kind else {
            return err($err.span(next.span));
        };
        ast::Ident {
            name: name.to_owned(),
            span: next.span,
        }
    }};
}

fn needs_semi(stmt: &ast::Statement) -> bool {
    let ast::StatementKind::Bare = stmt.kind else {
        return true;
    };
    expr_needs_semi(&stmt.expr)
}

fn expr_needs_semi(expr: &ast::Expr) -> bool {
    match &expr.body {
        ast::ExprInner::FnDefinition(_) => false,
        ast::ExprInner::Block(_) => false,
        ast::ExprInner::If(_cond, if_branch, None) => expr_needs_semi(if_branch),
        ast::ExprInner::If(_cond, _if, Some(else_branch)) => expr_needs_semi(else_branch),
        ast::ExprInner::While(_cond, body) => expr_needs_semi(body),
        _ => true,
    }
}

/* Statement-level parser */
impl<'a> Parser<'a> {
    fn parse_program(&mut self) -> ProgramResult {
        let body = self.parse_stmts()?;
        Ok(ast::Program { body })
    }

    /// Parse statements until a '}' or EOF.
    fn parse_stmts(&mut self) -> Result<Vec<ast::Statement>, Diag> {
        let mut stmts = vec![];
        loop {
            self.skip_semis()?;
            // Stop if there's a `}` or Eof
            if terminates_block(self.peek()?.kind) {
                return Ok(stmts);
            }
            // Parse the main expr.
            let stmt = self.parse_one_stmt()?;
            // ;
            if needs_semi(&stmt) {
                consume_token!(self, Semi, PE::ExpectedSemi);
            }
            stmts.push(stmt);
        }
    }

    #[cfg(test)]
    pub(crate) fn parse_one_stmt_for_tests(&mut self) -> Result<ast::Statement, Diag> {
        let stmt = self.parse_one_stmt()?;
        self.assert_stream_done()?;
        Ok(stmt)
    }

    fn parse_one_stmt(&mut self) -> Result<ast::Statement, Diag> {
        Ok(match self.peek()?.kind {
            Kw(Let) => {
                let let_token = self.next()?;
                // x
                let lhs = self.parse_lhs()?;
                // =
                consume_token!(self, Equals, PE::LetExpEquals { ident: lhs.ident });
                // y * 2
                let expr = self.parse_expr()?;
                ast::Statement {
                    span: span(let_token.span.lo, expr.span.hi),
                    kind: ast::StatementKind::Let(lhs),
                    expr,
                }
            }
            Kw(Ret) => {
                let ret_token = self.next()?;
                let expr = self.parse_expr()?;
                ast::Statement {
                    span: span(ret_token.span.lo, expr.span.hi),
                    kind: ast::StatementKind::Ret,
                    expr,
                }
            }
            _ => {
                let expr = self.parse_expr()?;
                ast::Statement {
                    span: expr.span,
                    kind: ast::StatementKind::Bare,
                    expr,
                }
            }
        })
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
    pub(crate) fn parse_expr_for_tests(&mut self) -> ExprResult {
        let expr = self.parse_expr()?;
        self.assert_stream_done()?;
        Ok(expr)
    }

    fn parse_expr(&mut self) -> ExprResult {
        self.parse_main(BindingPower::Top)
    }

    /// Parse an expression
    fn parse_main(&mut self, last_bp: BindingPower) -> ExprResult {
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
            Ident(s) => Ok(expr(ast::IdentExpr(s.to_owned()), start.span)),
            OpenDelim(Parenthesis) => {
                if let CloseDelim(Parenthesis) = self.peek()?.kind {
                    let close_paren = self.next()?;
                    let s = span(start.span.lo, close_paren.span.hi);
                    return Ok(expr(ast::Literal(ast::Lit::Unit), s));
                }
                let inner = self.parse_expr()?;
                let close_paren = consume_token!(
                    self,
                    CloseDelim(Parenthesis),
                    PE::OpenParenMissingCloseParen
                );
                let s = span(start.span.lo, close_paren.span.hi);
                Ok(expr(ast::Paren(inner), s))
            }
            BinOp(Plus) => err(PE::UnaryPlusDisallowed.span(start.span)),
            BinOp(Minus) => {
                let inner = self.parse_main(BindingPower::Prefix)?;
                let unop = respan(ast::UnaryOpKind::Neg, start.span);
                let s = span(start.span.lo, inner.span.hi);
                Ok(expr(ast::Unary(unop, inner), s))
            }
            Kw(Fn) => self.parse_fn(start),
            OpenDelim(CurlyBrace) => self.parse_block(start),
            Kw(If) => self.parse_if(start),
            Kw(While) => self.parse_while(start),
            Bang => {
                let inner = self.parse_main(BindingPower::Prefix)?;
                let unop = respan(ast::UnaryOpKind::Not, start.span);
                let s = span(start.span.lo, inner.span.hi);
                Ok(expr(ast::Unary(unop, inner), s))
            }
            BinOp(_) => err(PE::UnexpectedBinaryInitial.span(start.span)),
            CloseDelim(Parenthesis) => err(PE::UnmatchedCloseParen.span(start.span)),
            Eof => err(PE::UnexpectedEOF.span(start.span)),
            CloseDelim(CurlyBrace)
            | Kw(Else | Let | Ret | Mut)
            | Colon
            | Comma
            | ThinArrow
            | Semi
            | Equals => err(PE::GeneralUnexpected.span(start.span)),
            Whitespace | Invalid(_) => panic!("Whitespace should be skipped"),
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
            Equals => {
                // Shouldn't matter in valid code, but we want to parse
                // `1 + x = 5` as `(1 + x) = 5` to report error earlier.
                if BindingPower::Top < last_bp {
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
            Equals => Ok(self.parse_assign(*left)?),
            // Expected consequent_good to return `None` first, if this is reached.
            _ => panic!("Invariant violation: entered non-good consequent"),
        }
    }

    fn parse_binop(&mut self, left: Bexpr, tok: BinOpToken, sp: Span) -> ExprResult {
        // All operations are left-assocative.
        // Subtract one from the binding power here if you want right-associative.
        let pow = binop_power(tok);
        let right = self.parse_main(pow)?;
        // Special case: Don't allow 1 < 2 < 3
        if pow == BindingPower::Compare {
            if let ast::Binary(left_kind, ..) = left.body {
                if let ast::Compare(..) = left_kind.node {
                    err(PE::ComparatorChainDisallowed.span(sp))?;
                }
            }
        }
        let binop = respan(translate_binop(tok), sp);
        let s = span(left.span.lo, right.span.hi);
        Ok(expr(ast::Binary(binop, left, right), s))
    }

    fn parse_call(&mut self, left: Bexpr) -> ExprResult {
        let mut args = vec![];
        // disallow f(,)
        let after_open_paren = self.peek()?;
        if let Comma = after_open_paren.kind {
            return err(PE::FnBadComma.span(after_open_paren.span));
        }
        // main loop
        while maybe_starts_expr(self.peek()?.kind) {
            let arg = self.parse_expr()?;
            // , or )
            let after_arg = self.peek()?;
            // Don't need comma ',' if next token is ')'
            if try_consume_comma(after_arg.kind) {
                // Trailing comma, or comma between args
                consume_token!(self, Comma, PE::CallExpComma);
            }
            args.push(*arg);
        }
        let close_paren = consume_token!(self, CloseDelim(Parenthesis), PE::CallExpCloseParen);
        let s = span(left.span.lo, close_paren.span.hi);
        Ok(expr(ast::FnCall(left, args), s))
    }

    fn parse_assign(&mut self, lhs: ast::Expr) -> ExprResult {
        let ast::IdentExpr(name) = lhs.body else {
            return err(PE::AssignLHSMustBeIdent.span(lhs.span));
        };
        // Already saw the '='.
        let rhs = self.parse_expr()?;
        let s = span(lhs.span.lo, rhs.span.hi);
        let ident = ast::Ident {
            name,
            span: lhs.span,
        };
        Ok(expr(ast::Assign(ident, rhs), s))
    }

    fn parse_block(&mut self, start_curly: Token) -> ExprResult {
        let body = self.parse_stmts()?;
        let close_curly = consume_token!(
            self,
            CloseDelim(CurlyBrace),
            PE::BlockExpCloseCurly {
                open_curly: start_curly.span
            }
        );
        let s = span(start_curly.span.lo, close_curly.span.hi);
        Ok(expr(ast::Block(body), s))
    }

    fn parse_fn(&mut self, fn_token: Token) -> ExprResult {
        let fn_name = consume_ident!(self, PE::FnExpectedName);
        // (
        consume_token!(self, OpenDelim(Parenthesis), PE::FnExpOpenParen { fn_name });
        let mut params: Vec<ast::FunctionParam> = vec![];
        // disallow f(,)
        let after_open_paren = self.peek()?;
        if let Comma = after_open_paren.kind {
            return err(PE::FnBadComma.span(after_open_paren.span));
        }
        // main loop
        while maybe_starts_expr(self.peek()?.kind) {
            // x
            let param = consume_ident!(self, PE::FnExpParameter { fn_name });
            // :
            consume_token!(self, Colon, PE::FnExpColon { fn_name, param });
            // i64
            let type_name = self.parse_type()?;
            // , or )
            let after_param = self.peek()?;
            // Don't need comma ',' if next token is ')'
            if try_consume_comma(after_param.kind) {
                // Trailing comma, or comma between args
                consume_token!(self, Comma, PE::ParamExpComma);
            }
            params.push(ast::FunctionParam {
                name: param,
                param_type: type_name,
            });
        }
        // )
        consume_token!(
            self,
            CloseDelim(Parenthesis),
            PE::FnExpCloseParen { fn_name }
        );
        // ->
        consume_token!(self, ThinArrow, PE::FnExpThinArrow { fn_name });
        // i64
        let return_type = self.parse_type()?;
        // {
        consume_token!(self, OpenDelim(CurlyBrace), PE::FnExpOpenCurly { fn_name });
        // ret x * 2;
        let body = self.parse_stmts()?;
        // }
        let close_curly = consume_token!(
            self,
            CloseDelim(CurlyBrace),
            PE::FnExpCloseCurly { fn_name }
        );
        Ok(expr(
            ast::FnDefinition(ast::FunctionDefinition {
                fn_name,
                params,
                body,
                return_type,
            }),
            span(fn_token.span.lo, close_curly.span.hi),
        ))
    }

    fn parse_type(&mut self) -> ExprResult {
        let peeked = self.peek()?;
        if !maybe_starts_expr(peeked.kind) {
            err(PE::ExpType.span(peeked.span))?;
        }
        self.parse_expr()
    }

    fn parse_if(&mut self, if_token: Token) -> ExprResult {
        // (
        consume_token!(self, OpenDelim(Parenthesis), PE::IfExpOpenParen);
        // x > 2
        let cond = self.parse_expr()?;
        // )
        consume_token!(self, CloseDelim(Parenthesis), PE::IfExpCloseParen);
        let true_branch = self.parse_expr()?;
        let (false_branch, hi) = if let Kw(Else) = self.peek()?.kind {
            // else
            self.next()?;
            let false_branch = self.parse_expr()?;
            let hi = false_branch.span.hi;
            (Some(false_branch), hi)
        } else {
            (None, true_branch.span.hi)
        };
        let s = span(if_token.span.lo, hi);
        Ok(expr(ast::If(cond, true_branch, false_branch), s))
    }

    fn parse_while(&mut self, while_token: Token) -> ExprResult {
        // (
        consume_token!(self, OpenDelim(Parenthesis), PE::IfExpOpenParen);
        // x > 2
        let cond = self.parse_expr()?;
        // )
        consume_token!(self, CloseDelim(Parenthesis), PE::IfExpCloseParen);
        // body
        let body = self.parse_expr()?;
        let s = span(while_token.span.lo, body.span.hi);
        Ok(expr(ast::While(cond, body), s))
    }

    fn parse_lhs(&mut self) -> Result<ast::LetLHS, Diag> {
        let mut mutability = Mutability::No;
        // mut
        if let Kw(Mut) = self.peek()?.kind {
            self.next()?;
            mutability = Mutability::Yes;
        }
        // x
        let ident = consume_ident!(self, PE::LetExpName);
        Ok(ast::LetLHS { ident, mutability })
    }
}

fn binop_power(t: BinOpToken) -> BindingPower {
    match t {
        Plus | Minus => BindingPower::Add,
        Star | Slash | FloorDiv => BindingPower::Mul,
        Lt | LtEq | Gt | GtEq | Neq | EqEq => BindingPower::Compare,
        And => BindingPower::And,
        Or => BindingPower::Or,
    }
}

fn translate_binop(t: BinOpToken) -> ast::BinOpKind {
    match t {
        And => ast::And,
        Or => ast::Or,
        Lt => ast::Compare(ast::Lt),
        LtEq => ast::Compare(ast::LtEq),
        Gt => ast::Compare(ast::Gt),
        GtEq => ast::Compare(ast::GtEq),
        Neq => ast::Compare(ast::Neq),
        EqEq => ast::Compare(ast::Eq),
        Plus => ast::Add,
        Minus => ast::Sub,
        Star => ast::Mul,
        Slash => ast::TrueDiv,
        FloorDiv => ast::FloorDiv,
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
        | Kw(_) | Equals | Bang => false,
        Whitespace => panic!("Whitespace should be skipped"),
    }
}

fn try_consume_comma(tok: TokenKind) -> bool {
    match tok {
        Comma => true,
        _ => maybe_starts_expr(tok),
    }
}

/// This function must return false for `CloseDelim(Parenthesis)` and `Eof`, and
/// true for `Comma` and any valid initial token for an expression.
/// The behavior otherwise is arbitrary: `true` leads to "Expected )",
/// and `false` leads to "Expected ','"
fn maybe_starts_expr(tok: TokenKind) -> bool {
    match tok {
        BinOp(_) | OpenDelim(Parenthesis) | Literal(_) | Ident(_) | Kw(_) | Bang => true,
        Comma
        | OpenDelim(CurlyBrace)
        | Colon
        | Equals
        | CloseDelim(_)
        | Eof
        | ThinArrow
        | Invalid(_)
        | Semi => false,
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
        check_parsing("()", expect!["(1-2)Literal(Unit)"]);
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
                (1-3)Binary[TrueDiv(2)](
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
        check_parsing(
            "!!!x",
            expect![[r#"
                (1-4)Unary[Not(1)](
                    (2-4)Unary[Not(2)](
                        (3-4)Unary[Not(3)](
                            (4)Ident("x"),
                        ),
                    ),
                )"#]],
        );
    }

    #[test]
    fn precedence_adjacent() {
        // Top < Or
        check_parsing(
            "(x || y)",
            expect![[r#"
            (1-8)paren@(2-7)Binary[Or(4-5)](
                (2)Ident("x"),
                (7)Ident("y"),
            )"#]],
        );
        // Assign = Top < Or
        check_parsing(
            "x = y || z",
            expect![[r#"
                (1-10)Assign {
                    ident: (1)Ident("x"),
                    expr: (5-10)Binary[Or(7-8)](
                        (5)Ident("y"),
                        (10)Ident("z"),
                    ),
                }"#]],
        );
        check_parsing(
            "x || y = z",
            expect!["At 1-6: Left-hand-side of assignment must be an identifier, not a composite expression. For example: `x = 5;`"],
        );
        // Or < And
        check_parsing(
            "x && y || z && w",
            expect![[r#"
            (1-16)Binary[Or(8-9)](
                (1-6)Binary[And(3-4)](
                    (1)Ident("x"),
                    (6)Ident("y"),
                ),
                (11-16)Binary[And(13-14)](
                    (11)Ident("z"),
                    (16)Ident("w"),
                ),
            )"#]],
        );
        // And < Compare
        check_parsing(
            "x == w && y < z",
            expect![[r#"
            (1-15)Binary[And(8-9)](
                (1-6)Binary[Compare(Eq)(3-4)](
                    (1)Ident("x"),
                    (6)Ident("w"),
                ),
                (11-15)Binary[Compare(Lt)(13)](
                    (11)Ident("y"),
                    (15)Ident("z"),
                ),
            )"#]],
        );
        check_parsing(
            "y >= z && x != w",
            expect![[r#"
            (1-16)Binary[And(8-9)](
                (1-6)Binary[Compare(GtEq)(3-4)](
                    (1)Ident("y"),
                    (6)Ident("z"),
                ),
                (11-16)Binary[Compare(Neq)(13-14)](
                    (11)Ident("x"),
                    (16)Ident("w"),
                ),
            )"#]],
        );
        // Compare < Add
        check_parsing(
            "x + y == w + z",
            expect![[r#"
            (1-14)Binary[Compare(Eq)(7-8)](
                (1-5)Binary[Add(3)](
                    (1)Ident("x"),
                    (5)Ident("y"),
                ),
                (10-14)Binary[Add(12)](
                    (10)Ident("w"),
                    (14)Ident("z"),
                ),
            )"#]],
        );
        check_parsing(
            "x - y < w - z",
            expect![[r#"
            (1-13)Binary[Compare(Lt)(7)](
                (1-5)Binary[Sub(3)](
                    (1)Ident("x"),
                    (5)Ident("y"),
                ),
                (9-13)Binary[Sub(11)](
                    (9)Ident("w"),
                    (13)Ident("z"),
                ),
            )"#]],
        );
        // Add < Mul
        check_parsing(
            "x * y + z * w",
            expect![[r#"
            (1-13)Binary[Add(7)](
                (1-5)Binary[Mul(3)](
                    (1)Ident("x"),
                    (5)Ident("y"),
                ),
                (9-13)Binary[Mul(11)](
                    (9)Ident("z"),
                    (13)Ident("w"),
                ),
            )"#]],
        );
        check_parsing(
            "x / y - z / w",
            expect![[r#"
                (1-13)Binary[Sub(7)](
                    (1-5)Binary[TrueDiv(3)](
                        (1)Ident("x"),
                        (5)Ident("y"),
                    ),
                    (9-13)Binary[TrueDiv(11)](
                        (9)Ident("z"),
                        (13)Ident("w"),
                    ),
                )"#]],
        );
        check_parsing(
            "x // y - z // w",
            expect![[r#"
                (1-15)Binary[Sub(8)](
                    (1-6)Binary[FloorDiv(3-4)](
                        (1)Ident("x"),
                        (6)Ident("y"),
                    ),
                    (10-15)Binary[FloorDiv(12-13)](
                        (10)Ident("z"),
                        (15)Ident("w"),
                    ),
                )"#]],
        );
        // Mul < Prefix
        check_parsing(
            "-x * -y",
            expect![[r#"
            (1-7)Binary[Mul(4)](
                (1-2)Unary[Neg(1)](
                    (2)Ident("x"),
                ),
                (6-7)Unary[Neg(6)](
                    (7)Ident("y"),
                ),
            )"#]],
        );
        // Prefix < Call
        check_parsing(
            "-f(x)",
            expect![[r#"
            (1-5)Unary[Neg(1)](
                (2-5)call((2)Ident("f"))(
                    (4)Ident("x"),
                ),
            )"#]],
        );
        check_parsing(
            "f(-x)",
            expect![[r#"
                (1-5)call((1)Ident("f"))(
                    (3-4)Unary[Neg(3)](
                        (4)Ident("x"),
                    ),
                )"#]],
        );
        check_parsing(
            "f(if(x)y else z)",
            expect![[r#"
                (1-16)call((1)Ident("f"))(
                    (3-15)If {
                        cond: (6)Ident("x"),
                        true: (8)Ident("y"),
                        false: Some(
                            (15)Ident("z"),
                        ),
                    },
                )"#]],
        );
        check_parsing(
            "!f(x)",
            expect![[r#"
            (1-5)Unary[Not(1)](
                (2-5)call((2)Ident("f"))(
                    (4)Ident("x"),
                ),
            )"#]],
        );
    }

    #[test]
    fn precedence_misc() {
        // Mul < Call
        check_parsing(
            "x*y (x)",
            expect![[r#"
            (1-7)Binary[Mul(2)](
                (1)Ident("x"),
                (3-7)call((3)Ident("y"))(
                    (6)Ident("x"),
                ),
            )"#]],
        );
        // Call inner < Or
        check_parsing(
            "f(x || y)",
            expect![[r#"
            (1-9)call((1)Ident("f"))(
                (3-8)Binary[Or(5-6)](
                    (3)Ident("x"),
                    (8)Ident("y"),
                ),
            )"#]],
        );
    }

    #[test]
    fn associativity() {
        // Assign
        check_parsing(
            "x = y = z",
            expect![[r#"
                (1-9)Assign {
                    ident: (1)Ident("x"),
                    expr: (5-9)Assign {
                        ident: (5)Ident("y"),
                        expr: (9)Ident("z"),
                    },
                }"#]],
        );
        // Or
        check_parsing(
            "x || y || z",
            expect![[r#"
            (1-11)Binary[Or(8-9)](
                (1-6)Binary[Or(3-4)](
                    (1)Ident("x"),
                    (6)Ident("y"),
                ),
                (11)Ident("z"),
            )"#]],
        );
        // And
        check_parsing(
            "x && y && z",
            expect![[r#"
            (1-11)Binary[And(8-9)](
                (1-6)Binary[And(3-4)](
                    (1)Ident("x"),
                    (6)Ident("y"),
                ),
                (11)Ident("z"),
            )"#]],
        );
        // Compare
        check_parsing(
            "x == y == z",
            expect!["At 8-9: Chaining comparison operations is not yet supported."],
        );
        check_parsing(
            "x != y > z",
            expect!["At 8: Chaining comparison operations is not yet supported."],
        );
        check_parsing(
            "x < y != z",
            expect!["At 7-8: Chaining comparison operations is not yet supported."],
        );
        check_parsing(
            "x < y < z",
            expect!["At 7: Chaining comparison operations is not yet supported."],
        );
        check_parsing(
            "x < y <= z",
            expect!["At 7-8: Chaining comparison operations is not yet supported."],
        );
        check_parsing(
            "x >= y >= z",
            expect!["At 8-9: Chaining comparison operations is not yet supported."],
        );
        // Add
        check_parsing(
            "x + y + z",
            expect![[r#"
            (1-9)Binary[Add(7)](
                (1-5)Binary[Add(3)](
                    (1)Ident("x"),
                    (5)Ident("y"),
                ),
                (9)Ident("z"),
            )"#]],
        );
        check_parsing(
            "x - y - z",
            expect![[r#"
            (1-9)Binary[Sub(7)](
                (1-5)Binary[Sub(3)](
                    (1)Ident("x"),
                    (5)Ident("y"),
                ),
                (9)Ident("z"),
            )"#]],
        );
        // Mul
        check_parsing(
            "x * y / z",
            expect![[r#"
                (1-9)Binary[TrueDiv(7)](
                    (1-5)Binary[Mul(3)](
                        (1)Ident("x"),
                        (5)Ident("y"),
                    ),
                    (9)Ident("z"),
                )"#]],
        );
        check_parsing(
            "x / y * z",
            expect![[r#"
                (1-9)Binary[Mul(7)](
                    (1-5)Binary[TrueDiv(3)](
                        (1)Ident("x"),
                        (5)Ident("y"),
                    ),
                    (9)Ident("z"),
                )"#]],
        );
        check_parsing(
            "x / y / z",
            expect![[r#"
                (1-9)Binary[TrueDiv(7)](
                    (1-5)Binary[TrueDiv(3)](
                        (1)Ident("x"),
                        (5)Ident("y"),
                    ),
                    (9)Ident("z"),
                )"#]],
        );
        check_parsing(
            "x // y // z",
            expect![[r#"
                (1-11)Binary[FloorDiv(8-9)](
                    (1-6)Binary[FloorDiv(3-4)](
                        (1)Ident("x"),
                        (6)Ident("y"),
                    ),
                    (11)Ident("z"),
                )"#]],
        );
        check_parsing(
            "x * y * z",
            expect![[r#"
            (1-9)Binary[Mul(7)](
                (1-5)Binary[Mul(3)](
                    (1)Ident("x"),
                    (5)Ident("y"),
                ),
                (9)Ident("z"),
            )"#]],
        );
        // Prefix
        check_parsing(
            "!-!-!x",
            expect![[r#"
            (1-6)Unary[Not(1)](
                (2-6)Unary[Neg(2)](
                    (3-6)Unary[Not(3)](
                        (4-6)Unary[Neg(4)](
                            (5-6)Unary[Not(5)](
                                (6)Ident("x"),
                            ),
                        ),
                    ),
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
                    (5-25)Binary[TrueDiv(21)](
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
            "f(())",
            expect![[r#"
                (1-5)call((1)Ident("f"))(
                    (3-4)Literal(Unit),
                )"#]],
        );
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
            expect!["At 4: Expected ')' to end function arguments."],
        );
        check_parsing(
            "f(x ;",
            expect!["At 5: Expected ')' to end function arguments."],
        );
    }

    #[test]
    fn block_in_expr() {
        check_parsing(
            "1 + { ret 1 + 2; } + 6",
            expect![[r#"
                (1-22)Binary[Add(20)](
                    (1-18)Binary[Add(3)](
                        (1)Literal(Integer(1)),
                        (5-18)Block{
                            (7-15) ret (11-15)Binary[Add(13)](
                                (11)Literal(Integer(1)),
                                (15)Literal(Integer(2)),
                            ),
                        },
                    ),
                    (22)Literal(Integer(6)),
                )"#]],
        );
        check_parsing(
            "-{ let y = 3; ret y + 2; }",
            expect![[r#"
                (1-26)Unary[Neg(1)](
                    (2-26)Block{
                        (4-12) let y = (12)Literal(Integer(3)),
                        (15-23) ret (19-23)Binary[Add(21)](
                            (19)Ident("y"),
                            (23)Literal(Integer(2)),
                        ),
                    },
                )"#]],
        );
        check_parsing(
            "1+{ret 2;}+4",
            expect![[r#"
                (1-12)Binary[Add(11)](
                    (1-10)Binary[Add(2)](
                        (1)Literal(Integer(1)),
                        (3-10)Block{
                            (4-8) ret (8)Literal(Integer(2)),
                        },
                    ),
                    (12)Literal(Integer(4)),
                )"#]],
        )
    }

    #[test]
    fn if_expr_parse_errors() {
        check_parsing(
            "if x>2 {1} else {0}",
            expect!["At 4: Expected '(' for condition of 'if' statement. For example: `if (x > 5) 1 else 0`."],
        );
        check_parsing(
            "if (x>2 {1} else {0}",
            expect!["At 9: Expected ')' to close condition of 'if' statement. For example: `if (x > 5) 1 else 0`."],
        );
        check_parsing(
            "if () 1 else 0",
            // TODO-errormsg: Better message here
            expect!["At 5: What's this ')' doing here? I don't see a '('"],
        );
        check_parsing(
            "if (x) else 0",
            // TODO-errormsg: Better message here
            expect!["At 8-11: Unexpected token."],
        );
        check_parsing(
            "if (x) 1 else",
            expect!["At (!14,14!): Hold your horses. An EOF already?"],
        );
    }

    #[test]
    fn if_expr() {
        check_parsing(
            "if (x) 1",
            expect![[r#"
                (1-8)If {
                    cond: (5)Ident("x"),
                    true: (8)Literal(Integer(1)),
                    false: None,
                }"#]],
        );
        check_parsing(
            "if (x>2) 1 else 0",
            expect![[r#"
                (1-17)If {
                    cond: (5-7)Binary[Compare(Gt)(6)](
                        (5)Ident("x"),
                        (7)Literal(Integer(2)),
                    ),
                    true: (10)Literal(Integer(1)),
                    false: Some(
                        (17)Literal(Integer(0)),
                    ),
                }"#]],
        );
        check_parsing(
            "1 * if (x>2) 3 || 4 else 5 || 6",
            expect![[r#"
                (1-31)Binary[Mul(3)](
                    (1)Literal(Integer(1)),
                    (5-31)If {
                        cond: (9-11)Binary[Compare(Gt)(10)](
                            (9)Ident("x"),
                            (11)Literal(Integer(2)),
                        ),
                        true: (14-19)Binary[Or(16-17)](
                            (14)Literal(Integer(3)),
                            (19)Literal(Integer(4)),
                        ),
                        false: Some(
                            (26-31)Binary[Or(28-29)](
                                (26)Literal(Integer(5)),
                                (31)Literal(Integer(6)),
                            ),
                        ),
                    },
                )"#]],
        );
        check_parsing(
            "-if (a) 1 else 2",
            expect![[r#"
                (1-16)Unary[Neg(1)](
                    (2-16)If {
                        cond: (6)Ident("a"),
                        true: (9)Literal(Integer(1)),
                        false: Some(
                            (16)Literal(Integer(2)),
                        ),
                    },
                )"#]],
        );
    }

    #[test]
    fn while_expr_errors() {
        check_parsing(
            "while x>2 {1}",
            expect!["At 7: Expected '(' for condition of 'if' statement. For example: `if (x > 5) 1 else 0`."],
        );
        check_parsing(
            "while (x>2 {1}",
            expect!["At 12: Expected ')' to close condition of 'if' statement. For example: `if (x > 5) 1 else 0`."],
        );
        check_parsing(
            "while () 1",
            // TODO-errormsg: Better message here
            expect!["At 8: What's this ')' doing here? I don't see a '('"],
        );
        check_parsing("{ while(x); }", expect!["At 11: Unexpected token."]);
    }

    #[test]
    fn while_expr() {
        check_parsing(
            "while (x > 0) x = x - 1",
            expect![[r#"
                (1-23)While {
                    cond: (8-12)Binary[Compare(Gt)(10)](
                        (8)Ident("x"),
                        (12)Literal(Integer(0)),
                    ),
                    body: (15-23)Assign {
                        ident: (15)Ident("x"),
                        expr: (19-23)Binary[Sub(21)](
                            (19)Ident("x"),
                            (23)Literal(Integer(1)),
                        ),
                    },
                }"#]],
        );
        check_parsing(
            "while (x > 0) { x = x - 1; }",
            expect![[r#"
                (1-28)While {
                    cond: (8-12)Binary[Compare(Gt)(10)](
                        (8)Ident("x"),
                        (12)Literal(Integer(0)),
                    ),
                    body: (15-28)Block{
                        (17-25)Assign {
                            ident: (17)Ident("x"),
                            expr: (21-25)Binary[Sub(23)](
                                (21)Ident("x"),
                                (25)Literal(Integer(1)),
                            ),
                        },
                    },
                }"#]],
        );
    }
}

#[cfg(test)]
mod parser_stmt_tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check_parsing(input: &str, expect: Expect) {
        let actual: String = match Parser::new(input).parse_one_stmt_for_tests() {
            Ok(expr) => format!("{:#?}", expr),
            Err(diag) => format!("{:#?}", diag),
        };
        expect.assert_eq(&actual)
    }

    #[test]
    fn let_stmt() {
        check_parsing(
            "let mut x = 5",
            expect!["(1-13) let mut x = (13)Literal(Integer(5))"],
        );
        check_parsing("let x = 5", expect!["(1-9) let x = (9)Literal(Integer(5))"]);
        check_parsing("let x", expect!["At (!6,6!): Expected an '=', to provide an initial value for 'x'. For example: `let x = 5;`"]);
        check_parsing("()", expect!["(1-2)Literal(Unit)"]);
    }

    #[test]
    fn let_basic() {
        check_parsing(
            "let x = 5 + 3",
            expect![[r#"
            (1-13) let x = (9-13)Binary[Add(11)](
                (9)Literal(Integer(5)),
                (13)Literal(Integer(3)),
            )"#]],
        );
    }

    #[test]
    fn let_errors() {
        check_parsing("let;", expect!["At 4: Expected identifier. A 'let' statement must assign to a name. For example: `let x = 5;`"]);
        check_parsing("let x;", expect!["At 6: Expected an '=', to provide an initial value for 'x'. For example: `let x = 5;`"]);
        check_parsing("let x 5;", expect!["At 7: Expected an '=', to provide an initial value for 'x'. For example: `let x = 5;`"]);
        check_parsing(
            "let x =",
            expect!["At (!8,8!): Hold your horses. An EOF already?"],
        );
        check_parsing("let x =;", expect!["At 8: Unexpected token."]);
    }
}

#[cfg(test)]
mod parser_fn_tests {
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
                (5-32)FnDefinition[three]() -> (19-21)Ident("i64") {
                    (25-29) ret (29)Literal(Integer(3)),
                }
                (46-72)FnDefinition[four]() -> (59-61)Ident("i64") {
                    (65-69) ret (69)Literal(Integer(4)),
                }
                (86-88)Binary[Add(87)](
                    (86)Literal(Integer(4)),
                    (88)Literal(Integer(6)),
                )
            "#]],
        );
    }

    #[test]
    fn semicolon_required() {
        check_parsing(
            "let x=5 3;",
            expect!["At 9: Expected semicolon to end the statement"],
        );
        check_parsing(
            "let x={} 3;",
            expect!["At 10: Expected semicolon to end the statement"],
        );
        check_parsing(
            "let x=if(y)5 3;",
            expect!["At 14: Expected semicolon to end the statement"],
        );
        check_parsing(
            "let x=if(y)5 else if(z) 6 else 7 3;",
            expect!["At 34: Expected semicolon to end the statement"],
        );
        check_parsing(
            "ret x 3;",
            expect!["At 7: Expected semicolon to end the statement"],
        );
        check_parsing(
            "1+2 3;",
            expect!["At 5: Expected semicolon to end the statement"],
        );
        check_parsing(
            "-2 3;",
            expect!["At 4: Expected semicolon to end the statement"],
        );
        check_parsing(
            "x 3;",
            expect!["At 3: Expected semicolon to end the statement"],
        );
        check_parsing(
            "({}) 3;",
            expect!["At 6: Expected semicolon to end the statement"],
        );
        check_parsing(
            "if(y)5 3;",
            expect!["At 8: Expected semicolon to end the statement"],
        );
        check_parsing(
            "if(y){5;}else 4 3;",
            expect!["At 17: Expected semicolon to end the statement"],
        );
        check_parsing(
            "while(x) y 3",
            expect!["At 12: Expected semicolon to end the statement"],
        );
    }

    #[test]
    fn semicolon_optional() {
        check_parsing(
            "{x = 5;} 3;",
            expect![[r#"
                (1-8)Block{
                    (2-6)Assign {
                        ident: (2)Ident("x"),
                        expr: (6)Literal(Integer(5)),
                    },
                }
                (10)Literal(Integer(3))
            "#]],
        );
        check_parsing(
            "if(x){ret 5;} 3;",
            expect![[r#"
                (1-13)If {
                    cond: (4)Ident("x"),
                    true: (6-13)Block{
                        (7-11) ret (11)Literal(Integer(5)),
                    },
                    false: None,
                }
                (15)Literal(Integer(3))
            "#]],
        );
        check_parsing(
            "if(x)4 else if(y) 5 else {ret z;} 3;",
            expect![[r#"
                (1-33)If {
                    cond: (4)Ident("x"),
                    true: (6)Literal(Integer(4)),
                    false: Some(
                        (13-33)If {
                            cond: (16)Ident("y"),
                            true: (19)Literal(Integer(5)),
                            false: Some(
                                (26-33)Block{
                                    (27-31) ret (31)Ident("z"),
                                },
                            ),
                        },
                    ),
                }
                (35)Literal(Integer(3))
            "#]],
        );
        check_parsing(
            "while(x) {4;} 3;",
            expect![[r#"
                (1-13)While {
                    cond: (7)Ident("x"),
                    body: (10-13)Block{
                        (11)Literal(Integer(4)),
                    },
                }
                (15)Literal(Integer(3))
            "#]],
        );
    }

    #[test]
    fn fn_params() {
        check_parsing(
            "fn three() -> i64 { ret 3; }",
            expect![[r#"
                (1-28)FnDefinition[three]() -> (15-17)Ident("i64") {
                    (21-25) ret (25)Literal(Integer(3)),
                }
            "#]],
        );
        check_parsing(
            "fn double(x: i64) -> i64 { ret 2*x; }",
            expect![[r#"
                (1-37)FnDefinition[double](x: (14-16)Ident("i64")) -> (22-24)Ident("i64") {
                    (28-34) ret (32-34)Binary[Mul(33)](
                        (32)Literal(Integer(2)),
                        (34)Ident("x"),
                    ),
                }
            "#]],
        );
        check_parsing(
            "fn add(x: i64, y: i64) -> i64 { ret x + y; }",
            expect![[r#"
                (1-44)FnDefinition[add](x: (11-13)Ident("i64"), y: (19-21)Ident("i64")) -> (27-29)Ident("i64") {
                    (33-41) ret (37-41)Binary[Add(39)](
                        (37)Ident("x"),
                        (41)Ident("y"),
                    ),
                }
            "#]],
        );
        check_parsing(
            "fn branch(x: bool) -> () { if (x) {} else (); }",
            expect![[r#"
                (1-47)FnDefinition[branch](x: (14-17)Ident("bool")) -> (23-24)Literal(Unit) {
                    (28-44)If {
                        cond: (32)Ident("x"),
                        true: (35-36)Block{},
                        false: Some(
                            (43-44)Literal(Unit),
                        ),
                    },
                }
            "#]],
        );
        check_parsing(
            "fn ignore(x: ()) -> () { ret (); }",
            expect![[r#"
                (1-34)FnDefinition[ignore](x: (14-15)Literal(Unit)) -> (21-22)Literal(Unit) {
                    (26-31) ret (30-31)Literal(Unit),
                }
            "#]],
        );
        check_parsing(
            "fn double(x: (i64)) -> (i64) { ret 2*x; }",
            expect![[r#"
                (1-41)FnDefinition[double](x: (14-18)paren@(15-17)Ident("i64")) -> (24-28)paren@(25-27)Ident("i64") {
                    (32-38) ret (36-38)Binary[Mul(37)](
                        (36)Literal(Integer(2)),
                        (38)Ident("x"),
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
                (1-38)FnDefinition[double](x: (14-16)Ident("i64")) -> (23-25)Ident("i64") {
                    (29-35) ret (33-35)Binary[Mul(34)](
                        (33)Literal(Integer(2)),
                        (35)Ident("x"),
                    ),
                }
            "#]],
        );
        check_parsing(
            "fn add(x: i64, y: i64,) -> i64 { ret x + y; }",
            expect![[r#"
                (1-45)FnDefinition[add](x: (11-13)Ident("i64"), y: (19-21)Ident("i64")) -> (28-30)Ident("i64") {
                    (34-42) ret (38-42)Binary[Add(40)](
                        (38)Ident("x"),
                        (42)Ident("y"),
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
                (1-17)FnDefinition[f]() -> (11-13)Ident("i64") {}
            "#]],
        );
        check_parsing(
            "fn f() -> i64 { let x = y; }",
            expect![[r#"
                (1-28)FnDefinition[f]() -> (11-13)Ident("i64") {
                    (17-25) let x = (25)Ident("y"),
                }
            "#]],
        );
        check_parsing(
            "fn f() -> i64 { f(f(f(f()))); }",
            expect![[r#"
                (1-31)FnDefinition[f]() -> (11-13)Ident("i64") {
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
                (1-35)FnDefinition[f]() -> (11-13)Ident("i64") {
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
            expect!["At 10: Expected a type here. Try 'i64' or '()'."],
        );
        check_parsing(
            "fn add(x: i64 y: i64)",
            expect!["At 15: Expected ',' after function parameter."],
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

    #[test]
    fn top_level_block() {
        check_parsing(
            "{ret 1 + 2;};",
            expect![[r#"
                (1-12)Block{
                    (2-10) ret (6-10)Binary[Add(8)](
                        (6)Literal(Integer(1)),
                        (10)Literal(Integer(2)),
                    ),
                }
            "#]],
        );
    }
}
