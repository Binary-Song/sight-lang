use crate::ast::*;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::parser::ParseErr;
use crate::parser::Parser;
use crate::span::Span;
use function_name::named;

impl<'a> Parser<'a> {
    #[named]
    fn primitive_expr(&mut self) -> Result<Expr, ParseErr> {
        let rule = function_name!();
        let token = self.expect_any(&[TokenType::Ident, TokenType::IntLit], rule);
        match token {
            Ok(Token::Ident(name, span)) => Ok(Expr::Var { name, span }),
            Ok(Token::IntLit(lexeme, span)) => Ok(Expr::Int {
                value: lexeme.parse::<i32>().unwrap(),
                span,
            }),
            _ => panic!(
                "Logic error: expected primitive_expr token, got {:?}",
                token
            ),
        }
    }

    #[named]
    fn primary_expr(&mut self) -> Result<Expr, ParseErr> {
        let rule: &'static str = function_name!();
        let parsers: &[&dyn Fn(&mut Self) -> Result<Expr, ParseErr>] = &[
            // primitive expr parser
            &|parser: &mut Parser<'a>| parser.primitive_expr(),
            // parenthesized expr parser
            &|parser| parser.paren(rule, &|parser| parser.expr()),
            // block
            &|parser| parser.block().map(|block| Expr::Block(Box::new(block))),
        ];
        self.ll1_try_parse(parsers)
    }

    #[named]
    fn op_mul_div(&mut self) -> Result<Expr, ParseErr> {
        self.left_assoc_infix_op(
            &[TokenType::Star, TokenType::Slash],
            function_name!(),
            &|p: &mut Self| p.primary_expr(),
            &|lhs: Expr, rhs: Expr, token: Token| {
                let op = match token.token_type() {
                    TokenType::Star => BinaryOp::Mul,
                    TokenType::Slash => BinaryOp::Div,
                    _ => unreachable!("Logic error: expected Plus or Minus token, got {:?}", token),
                };
                let span = (lhs.span().0, rhs.span().1);
                let op_span = (token.span().0, token.span().1);
                Expr::BinaryOp {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span,
                    op_span,
                }
            },
        )
    }

    #[named]
    fn op_add_sub(&mut self) -> Result<Expr, ParseErr> {
        self.left_assoc_infix_op(
            &[TokenType::Plus, TokenType::Minus],
            function_name!(),
            &|p: &mut Self| p.op_mul_div(),
            &|lhs: Expr, rhs: Expr, token: Token| {
                let op = match token.token_type() {
                    TokenType::Plus => BinaryOp::Add,
                    TokenType::Minus => BinaryOp::Sub,
                    _ => unreachable!("Logic error: expected Plus or Minus token, got {:?}", token),
                };
                let span = (lhs.span().0, rhs.span().1);
                let op_span = (token.span().0, token.span().1);
                Expr::BinaryOp {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span,
                    op_span,
                }
            },
        )
    }

    fn op_expr(&mut self) -> Result<Expr, ParseErr> {
        self.op_add_sub()
    }

    pub fn expr(&mut self) -> Result<Expr, ParseErr> {
        self.op_expr()
    }
}
