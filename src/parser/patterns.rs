use crate::ast::*;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::parser::ParseErr;
use crate::parser::Parser;
use crate::span::Span;
use function_name::named;
use tracing::instrument;
use crate::parser::types::Prec as TypePrec;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Prec {
    Variable,
    Primary,
    Tuple,
    Max,
}

impl Prec {
    pub fn sub_by_one(self) -> Self {
        match self {
            Prec::Variable => Prec::Variable,
            Prec::Primary => Prec::Variable,
            Prec::Tuple => Prec::Primary,
            Prec::Max => Prec::Tuple,
        }
    }
}

impl<'a> Parser<'a> {
    #[named]
    #[instrument(ret)]
    fn variable_pattern(&mut self) -> Result<Pattern, ParseErr> {
        let rule = function_name!();
        let token = self.expect(TokenType::Ident, rule)?;
        let span = token.span();
        let name = if let Token::Ident(name, ..) = token {
            name
        } else {
            panic!("Logic error");
        };
        let _colon = self.expect(TokenType::Colon, rule)?;
        // We have to start from "below tuple types". Otherwise 
        // (a: int, b: int) will be parsed as (a: (int, b)...)
        let type_anno =
            self.type_expr_with_max_prec(TypePrec::Tuple.sub_by_one())?;
        Ok(Pattern::Var {
            name: name,
            ty: type_anno,
            span: span,
        })
    }

    #[named]
    #[instrument(ret)]
    fn primary_pattern(&mut self) -> Result<Pattern, ParseErr> {
        let rule: &'static str = function_name!();
        let parsers: &[&dyn Fn(&mut Self) -> Result<Pattern, ParseErr>] = &[
            // primitive expr parser
            &|parser: &mut Parser<'a>| parser.pattern_with_max_prec(Prec::Primary.sub_by_one()),
            // unit parser
            &|parser| {
                Ok(Pattern::Unit {
                    span: parser.unit()?,
                })
            },
            // parenthesized expr parser
            &|parser| parser.paren(rule, &|parser| parser.pattern()),
        ];
        self.ll1_try_parse(parsers)
    }

    #[named]
    #[instrument(ret)]
    fn tuple_pattern(&mut self) -> Result<Pattern, ParseErr> {
        let rule = function_name!();
        let parse_operand = |s: &mut Self| s.pattern_with_max_prec(Prec::Tuple.sub_by_one());
        let combine_operands = |oprs: Vec<Pattern>| -> Pattern {
            let span = (
                oprs.first().unwrap().span().0,
                oprs.last().unwrap().span().1,
            );
            Pattern::Tuple { elems: oprs, span }
        };
        self.variadic_op(TokenType::Comma, rule, parse_operand, combine_operands)
    }

    #[instrument(ret)]
    pub fn pattern(&mut self) -> Result<Pattern, ParseErr> {
        self.pattern_with_max_prec(Prec::Max.sub_by_one())
    }

    pub fn pattern_with_max_prec(&mut self, max_prec: Prec) -> Result<Pattern, ParseErr> {
        match max_prec {
            Prec::Variable => self.variable_pattern(),
            Prec::Primary => self.primary_pattern(),
            Prec::Tuple => self.tuple_pattern(),
            Prec::Max => self.pattern(), // Max is the same as Tuple in this context
        }
    }
}
