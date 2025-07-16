use crate::{
    ast::{Expr, Pattern, Stmt, TypeExpr},
    lexer::Token,
};
use sight_macros::LiteralValue;

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct Span(pub usize, pub usize);

pub trait HasSpan {
    fn span(self: &Self) -> Option<Span>;
}

impl HasSpan for Expr {
    fn span(self: &Expr) -> Option<Span> {
        match self {
            Expr::Unit { span }
            | Expr::Int { span, .. }
            | Expr::Bool { span, .. }
            | Expr::Var { span, .. }
            | Expr::UnaryOp { span, .. }
            | Expr::BinaryOp { span, .. }
            | Expr::App { span, .. }
            | Expr::Tuple { span, .. } => span.clone(),
            Expr::Block(b) => b.span.clone(),
        }
    }
}

impl HasSpan for TypeExpr {
    fn span(self: &Self) -> Option<Span> {
        match self {
            TypeExpr::Unit { span, .. }
            | TypeExpr::Bool { span, .. }
            | TypeExpr::Int { span, .. }
            | TypeExpr::Arrow { span, .. }
            | TypeExpr::Tuple { span, .. } => span.clone(),
        }
    }
}

impl HasSpan for Token {
    fn span(&self) -> Option<Span> {
        match self {
            Token::EqEq(span)
            | Token::NotEq(span)
            | Token::Le(span)
            | Token::Ge(span)
            | Token::AndAnd(span)
            | Token::OrOr(span)
            | Token::Arrow(span)
            | Token::FatArrow(span)
            | Token::Plus(span)
            | Token::Minus(span)
            | Token::Star(span)
            | Token::Slash(span)
            | Token::Percent(span)
            | Token::Not(span)
            | Token::Eq(span)
            | Token::Lt(span)
            | Token::Gt(span)
            | Token::LParen(span)
            | Token::RParen(span)
            | Token::LBrace(span)
            | Token::RBrace(span)
            | Token::Comma(span)
            | Token::Semicolon(span)
            | Token::Colon(span)
            | Token::Dot(span)
            | Token::IntLit(_, span)
            | Token::True(span)
            | Token::False(span)
            | Token::Fn(span)
            | Token::Let(span)
            | Token::Bool(span)
            | Token::Int(span)
            | Token::Ident(_, span)
            | Token::BadUtf8Char(_, span)
            | Token::Eof(span) => Some(span.clone()),
        }
    }
}

impl HasSpan for Pattern {
    fn span(self: &Self) -> Option<Span> {
        match self {
            Pattern::Unit { span } | Pattern::Var { span, .. } | Pattern::Tuple { span, .. } => {
                span.clone()
            }
        }
    }
}

impl HasSpan for Stmt {
    fn span(self: &Self) -> Option<Span> {
        match self {
            Stmt::Let { span, .. } => span.clone(),
            Stmt::Func(func) => func.span.clone(),
            Stmt::Block(block) => block.span.clone(),
            Stmt::Expr { span, .. } => span.clone(),
            Stmt::Empty { span } => span.clone(),
        }
    }
}
