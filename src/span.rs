
use crate::{
    ast::{Expr, Term, TypeExpr, Pattern},
    lexer::Token,
};

pub trait Span {
    fn span(self: &Self) -> (usize, usize);
}

impl Span for Expr {
    fn span(self: &Expr) -> (usize, usize) {
        match self {
            Expr::Unit { span }
            | Expr::Int { span, .. }
            | Expr::Bool { span, .. }
            | Expr::Var { span, .. }
            | Expr::UnaryOp { span, .. }
            | Expr::BinaryOp { span, .. }
            | Expr::Seq { span, .. }
            | Expr::App { span, .. }
            | Expr::Func { span, .. }
            | Expr::Let { span, .. }
            | Expr::Tuple { span, .. } => *span,
        }
    }
}

impl Span for TypeExpr {
    fn span(self: &Self) -> (usize, usize) {
        match self {
            TypeExpr::Bool { span } => *span,
            TypeExpr::Int { span } => *span,
            TypeExpr::Arrow { span, .. } => *span,
            TypeExpr::Tuple { span, .. } => *span,
        }
    }
}

impl Span for Term {
    fn span(self: &Self) -> (usize, usize) {
        match self {
            Term::Lit { span, .. } => *span,
            Term::Var { span, .. } => *span,
            Term::App { span, .. } => *span,
            Term::Func { span, .. } => *span,
            Term::Let { span, .. } => *span,
            Term::Seq { span, .. } => *span,
            Term::Op { span, .. } => *span,
            Term::Tuple { span, .. } => *span,
        }
    }
}

impl Span for Token {
    fn span(&self) -> (usize, usize) {
        match self {
            Token::EqEq(span) => *span,
            Token::NotEq(span) => *span,
            Token::Le(span) => *span,
            Token::Ge(span) => *span,
            Token::AndAnd(span) => *span,
            Token::OrOr(span) => *span,
            Token::Arrow(span) => *span,
            Token::FatArrow(span) => *span,
            Token::Plus(span) => *span,
            Token::Minus(span) => *span,
            Token::Star(span) => *span,
            Token::Slash(span) => *span,
            Token::Percent(span) => *span,
            Token::Not(span) => *span,
            Token::Assign(span) => *span,
            Token::Lt(span) => *span,
            Token::Gt(span) => *span,
            Token::LParen(span) => *span,
            Token::RParen(span) => *span,
            Token::LBrace(span) => *span,
            Token::RBrace(span) => *span,
            Token::Comma(span) => *span,
            Token::Semicolon(span) => *span,
            Token::Colon(span) => *span,
            Token::Dot(span) => *span,
            Token::IntLit(_, span) => *span,
            Token::True(span) => *span,
            Token::False(span) => *span,
            Token::Fn(span) => *span,
            Token::Let(span) => *span,
            Token::Bool(span) => *span,
            Token::Int(span) => *span,
            Token::Ident(_, span) => *span,
        }
    }
}

impl Span for Pattern {
    fn span(self: &Self) -> (usize, usize) {
        match self {
            Pattern::Var { span, .. } => *span,
            Pattern::Tuple { span, .. } => *span,
        }
    }
}