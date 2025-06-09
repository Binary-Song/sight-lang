use crate::{
    ast::{typed, Expr, Pattern, Stmt, TypeExpr},
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
            | Expr::App { span, .. }
            | Expr::Tuple { span, .. } => *span,
            Expr::Block(b) => b.span,
        }
    }
}

impl Span for TypeExpr {
    fn span(self: &Self) -> (usize, usize) {
        match self {
            TypeExpr::Unit { span, .. }
            | TypeExpr::Bool { span, .. }
            | TypeExpr::Int { span, .. }
            | TypeExpr::Arrow { span, .. }
            | TypeExpr::Tuple { span, .. } => *span,
        }
    }
}

impl Span for typed::Expr {
    fn span(self: &Self) -> (usize, usize) {
        match self {
            typed::Expr::Lit { span, .. }
            | typed::Expr::Var { span, .. }
            | typed::Expr::Let { span, .. }
            | typed::Expr::Seq { span, .. }
            | typed::Expr::Tuple { span, .. }
            | typed::Expr::Application { span, .. } => *span,
            typed::Expr::Func { func, .. } => func.span,
        }
    }
}

impl Span for Token {
    fn span(&self) -> (usize, usize) {
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
            | Token::Eof(span) => *span,
        }
    }
}

impl Span for Pattern {
    fn span(self: &Self) -> (usize, usize) {
        match self {
            Pattern::Unit { span } |
            Pattern::Var { span, .. } |
            Pattern::Tuple { span, .. } => *span,
        }
    }
}

impl Span for Stmt {
    fn span(self: &Self) -> (usize, usize) {
        match self {
            Stmt::Let { span, .. } => *span,
            Stmt::Func(func) => func.span,
            Stmt::Block(block) => block.span,
            Stmt::Expr { span, .. } => *span,
            Stmt::Empty { span } => *span,
        }
    }
}
