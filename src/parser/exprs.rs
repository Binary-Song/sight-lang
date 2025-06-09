use crate::ast::*;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::parser::ParseErr;
use crate::parser::Parser;
use crate::parser::PeekerResult;
use crate::span::Span;
use function_name::named;
use tracing::{info, instrument};

pub enum Prec {
    Primitive,
    Primary,
    OpMulDiv,
    OpAddSub,
    Tuple,
    Max,
}

impl Prec {
    pub fn sub_by_one(self) -> Self {
        match self {
            Prec::Primitive => Prec::Primitive,
            Prec::Primary => Prec::Primitive,
            Prec::OpMulDiv => Prec::Primary,
            Prec::OpAddSub => Prec::OpMulDiv,
            Prec::Tuple => Prec::OpAddSub,
            Prec::Max => Prec::Tuple,
        }
    }
}

impl<'a> Parser<'a> {
    #[named]
    #[instrument(ret)]
    fn primitive_expr(&mut self) -> Result<Expr, ParseErr> {
        let rule = function_name!();
        let token = self.expect_any(&[TokenType::Ident, TokenType::IntLit], rule)?;
        match token {
            Token::Ident(name, span) => Ok(Expr::Var { name, span }),
            Token::IntLit(lexeme, span) => Ok(Expr::Int {
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
    #[instrument(ret)]
    fn primary_expr(&mut self) -> Result<Expr, ParseErr> {
        let rule: &'static str = function_name!();
        let parsers: &[&dyn Fn(&mut Self) -> Result<Expr, ParseErr>] = &[
            // primitive expr parser
            &|parser: &mut Parser<'a>| parser.expr_with_max_prec(Prec::Primary.sub_by_one()),
            // unit parser
            &|parser| {
                let mut peeker = {
                    // the peeker state
                    let mut seen_lparen = false;
                    // the peeker
                    move |token: &Token| {
                        if !seen_lparen && token.token_type() == TokenType::LParen {
                            seen_lparen = true;
                            PeekerResult::Continue
                        } else if seen_lparen && token.token_type() == TokenType::RParen {
                            PeekerResult::Success
                        } else {
                            PeekerResult::Fail
                        }
                    }
                };
                let tokens = parser.expect_any_with_peeker(
                    &mut peeker,
                    "the unit expr `()`",
                    "unit_expr",
                )?;
                Ok(Expr::Unit {
                    span: (tokens[0].span().0, tokens.last().unwrap().span().1),
                })
            },
            // parenthesized expr parser
            &|parser| parser.paren(rule, &|parser| parser.expr()),
            // block
            &|parser| parser.block().map(|block| Expr::Block(Box::new(block))),
        ];
        self.ll1_try_parse(parsers)
    }

    #[named]
    #[instrument(ret)]
    fn op_mul_div(&mut self) -> Result<Expr, ParseErr> {
        self.left_assoc_infix_op(
            &[TokenType::Star, TokenType::Slash],
            function_name!(),
            &|p: &mut Self| p.expr_with_max_prec(Prec::OpMulDiv.sub_by_one()),
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
    #[instrument(ret)]
    fn op_add_sub(&mut self) -> Result<Expr, ParseErr> {
        self.left_assoc_infix_op(
            &[TokenType::Plus, TokenType::Minus],
            function_name!(),
            &|p: &mut Self| p.expr_with_max_prec(Prec::OpAddSub.sub_by_one()),
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

    #[named]
    #[instrument(ret)]
    fn tuple_expr(&mut self) -> Result<Expr, ParseErr> {
        let rule = function_name!();
        let parse_operand = |s: &mut Self| s.expr_with_max_prec(Prec::Tuple.sub_by_one());
        let combine_operands = |oprs: Vec<Expr>| -> Expr {
            let span = (
                oprs.first().unwrap().span().0,
                oprs.last().unwrap().span().1,
            );
            Expr::Tuple { elems: oprs, span }
        };
        self.variadic_op(TokenType::Comma, rule, parse_operand, combine_operands)
    }

    #[instrument(ret)]
    pub fn expr(&mut self) -> Result<Expr, ParseErr> {
        self.expr_with_max_prec(Prec::Max.sub_by_one())
    }

    pub fn expr_with_max_prec(&mut self, max_prec: Prec) -> Result<Expr, ParseErr> {
        match max_prec {
            Prec::Primitive => self.primitive_expr(),
            Prec::Primary => self.primary_expr(),
            Prec::OpMulDiv => self.op_mul_div(),
            Prec::OpAddSub => self.op_add_sub(),
            Prec::Tuple => self.tuple_expr(),
            Prec::Max => self.expr(),
        }
    }
}
