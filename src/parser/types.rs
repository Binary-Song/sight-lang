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

impl<'a> Parser<'a> {
    #[named]
    fn primitive_type_expr(&mut self) -> Result<TypeExpr, ParseErr> {
        let rule = function_name!();
        let token = self.expect_any(&[TokenType::Bool, TokenType::Int], rule)?;
        match token {
            Token::Bool(_) => Ok(TypeExpr::Bool { span: token.span() }),
            Token::Int(_) => Ok(TypeExpr::Int { span: token.span() }),
            _ => panic!("Logic error: expected primitive type token, got {:?}", token),
        }
    }

    #[named]
    fn primary_type_expr(&mut self) -> Result<TypeExpr, ParseErr> {
        let rule: &'static str = function_name!();
        let parsers: &[&dyn Fn(&mut Self) -> Result<TypeExpr, ParseErr>] = &[
            // primitive parser
            &|parser| parser.primitive_type_expr(),
            // parenthesized parser
            &|parser| parser.paren(rule, &|parser| parser.type_expr()),
        ];
        self.ll1_try_parse(parsers)
    }

    #[named]
    fn arrow_type_expr(&mut self) -> Result<TypeExpr, ParseErr> {
        let rule = function_name!();
        let parse_operand = |s: &mut Self| s.primary_type_expr();
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
    fn tuple_type_expr(&mut self) -> Result<TypeExpr, ParseErr> {
        let rule = function_name!();
        let parse_operand = |s: &mut Self| s.arrow_type_expr();
        let combine_operands = |oprs: Vec<TypeExpr>| -> TypeExpr {
            let span = (
                oprs.first().unwrap().span().0,
                oprs.last().unwrap().span().1,
            );
            TypeExpr::Tuple { elems: oprs, span }
        };
        self.variadic_op(TokenType::Comma, rule, parse_operand, combine_operands)
    }

    pub fn type_expr(&mut self) -> Result<TypeExpr, ParseErr> {
        self.tuple_type_expr()
    }
}
