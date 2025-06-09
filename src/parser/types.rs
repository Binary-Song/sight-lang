use crate::ast::*;
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::parser::ParseErr;
use crate::parser::Parser;
use crate::span::Span;
use function_name::named;
pub use lalrpop_util::lalrpop_mod;
use std::collections::VecDeque;
use std::vec;
use tracing::instrument;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Prec {
    Primitive,
    Primary,
    Arrow,
    Tuple,
    Max,
}

impl Prec {
    pub fn sub_by_one(self) -> Self {
        match self {
            Prec::Primitive => Prec::Primitive,
            Prec::Primary => Prec::Primitive,
            Prec::Arrow => Prec::Primary,
            Prec::Tuple => Prec::Arrow,
            Prec::Max => Prec::Tuple,
        }
    }
}

impl<'a> Parser<'a> {
    #[named]
    #[instrument(ret)]
    fn primitive_type_expr(&mut self) -> Result<TypeExpr, ParseErr> {
        let rule = function_name!();
        let token = self.expect_any(&[TokenType::Bool, TokenType::Int], rule)?;
        match token {
            Token::Bool(_) => Ok(TypeExpr::Bool { span: token.span() }),
            Token::Int(_) => Ok(TypeExpr::Int { span: token.span() }),
            _ => panic!(
                "Logic error: expected primitive type token, got {:?}",
                token
            ),
        }
    }

    #[named]
    #[instrument(ret)]
    fn primary_type_expr(&mut self) -> Result<TypeExpr, ParseErr> {
        let rule: &'static str = function_name!();
        let parsers: &[&dyn Fn(&mut Self) -> Result<TypeExpr, ParseErr>] = &[
            // primitive parser
            &|parser| parser.type_expr_with_max_prec(Prec::Primary.sub_by_one()),
            // unit parser
            &|parser| {
                Ok(TypeExpr::Unit {
                    span: parser.unit()?,
                })
            },
            // parenthesized parser
            &|parser| parser.paren(rule, &|parser| parser.type_expr()),
        ];
        self.ll1_try_parse(parsers)
    }

    #[named]
    #[instrument(ret)]
    fn arrow_type_expr(&mut self) -> Result<TypeExpr, ParseErr> {
        let rule = function_name!();
        let parse_operand = |s: &mut Self| s.type_expr_with_max_prec(Prec::Arrow.sub_by_one());
        let combine_operands = |lhs: TypeExpr, rhs: TypeExpr, tok| -> TypeExpr {
            let span = (lhs.span().0, rhs.span().1);
            TypeExpr::Arrow {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span,
            }
        };
        self.right_assoc_infix_op(&[TokenType::Arrow], rule, &parse_operand, &combine_operands)
    }

    #[named]
    #[instrument(ret)]
    fn tuple_type_expr(&mut self) -> Result<TypeExpr, ParseErr> {
        let rule = function_name!();
        let parse_operand = |s: &mut Self| s.type_expr_with_max_prec(Prec::Tuple.sub_by_one());
        let combine_operands = |oprs: Vec<TypeExpr>| -> TypeExpr {
            let span = (
                oprs.first().unwrap().span().0,
                oprs.last().unwrap().span().1,
            );
            TypeExpr::Tuple { elems: oprs, span }
        };
        self.variadic_op(TokenType::Comma, rule, parse_operand, combine_operands)
    }

    #[instrument(ret)]
    pub fn type_expr(&mut self) -> Result<TypeExpr, ParseErr> {
        self.type_expr_with_max_prec(Prec::Max.sub_by_one())
    }

    pub fn type_expr_with_max_prec(&mut self, max_prec: Prec) -> Result<TypeExpr, ParseErr> {
        match max_prec {
            Prec::Primitive => self.primitive_type_expr(),
            Prec::Primary => self.primary_type_expr(),
            Prec::Arrow => self.arrow_type_expr(),
            Prec::Tuple => self.tuple_type_expr(),
            Prec::Max => self.type_expr(),
        }
    }
}
