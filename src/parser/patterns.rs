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

    #[instrument(ret)]
    pub fn pattern_with_max_prec(&mut self, max_prec: Prec) -> Result<Pattern, ParseErr> {
        match max_prec {
            Prec::Variable => self.variable_pattern(),
            Prec::Primary => self.primary_pattern(),
            Prec::Tuple => self.tuple_pattern(),
            Prec::Max => self.pattern(), // Max is the same as Tuple in this context
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_pattern(input: &str) -> Result<Pattern, ParseErr> {
        let mut parser = Parser::new(input);
        parser.pattern()
    }

    #[test]
    fn test_variable_pattern_simple() {
        let pat = parse_pattern("x: Int").unwrap();
        match pat {
            Pattern::Var { name, .. } => assert_eq!(name, "x"),
            _ => panic!("Expected variable pattern"),
        }
    }

    #[test]
    fn test_tuple_pattern_two_elements() {
        let pat = parse_pattern("a: Int, b: Bool").unwrap();
        match pat {
            Pattern::Tuple { elems, .. } => {
                assert_eq!(elems.len(), 2);
                match &elems[0] {
                    Pattern::Var { name, .. } => assert_eq!(name, "a"),
                    _ => panic!("Expected variable pattern"),
                }
                match &elems[1] {
                    Pattern::Var { name, .. } => assert_eq!(name, "b"),
                    _ => panic!("Expected variable pattern"),
                }
            }
            _ => panic!("Expected tuple pattern"),
        }
    }

    #[test]
    fn test_nested_tuple_pattern() {
        let pat = parse_pattern("x: Int, (y: Bool, z: String)").unwrap();
        match pat {
            Pattern::Tuple { elems, .. } => {
                assert_eq!(elems.len(), 2);
                match &elems[1] {
                    Pattern::Tuple { elems: inner, .. } => {
                        assert_eq!(inner.len(), 2);
                        match &inner[0] {
                            Pattern::Var { name, .. } => assert_eq!(name, "y"),
                            _ => panic!("Expected variable pattern"),
                        }
                        match &inner[1] {
                            Pattern::Var { name, .. } => assert_eq!(name, "z"),
                            _ => panic!("Expected variable pattern"),
                        }
                    }
                    _ => panic!("Expected inner tuple pattern"),
                }
            }
            _ => panic!("Expected tuple pattern"),
        }
    }

    #[test]
    fn test_pattern_error_on_missing_colon() {
        let err = parse_pattern("x Int").unwrap_err();
        assert!(format!("{:?}", err).contains("Colon"));
    }

    #[test]
    fn test_pattern_error_on_empty_input() {
        let err = parse_pattern("").unwrap_err();
        assert!(format!("{:?}", err).contains("Ident"));
    }
}
