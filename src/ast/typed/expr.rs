use crate::ast::raw::Stmt;
use crate::ast::span::*;
use crate::ast::typed::binding::Binding;
use crate::ast::typed::r#type::Type;
use crate::{container::*, sum_id};
use sight_macros::LiteralValue;
use std::marker::PhantomData;

pub type ExprSumId = sum_id!(
    LiteralExpr,
    VariableExpr,
    ApplicationExpr,
    BlockExpr,
    TupleExpr,
    ProjectionExpr
);

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub enum Literal {
    Int(i32),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct LiteralExpr {
    pub value: Literal,
    pub span: Option<Span>,
    pub ty: Id<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct VariableExpr {
    pub target: Id<Binding>,
    pub name: Id<String>,
    pub ty: Id<Type>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct ApplicationExpr {
    pub callee: ExprSumId,
    pub arg: ExprSumId,
    pub ty: Id<Type>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct BlockExpr {
    pub block: Id<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct TupleExpr {
    pub elems: Vec<ExprSumId>,
    pub span: Option<Span>,
    pub ty: Id<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct ProjectionExpr {
    pub target: Id<ExprSumId>,
    pub index: usize,
    pub span: Option<Span>,
    pub ty: Id<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue, Hash)]
pub struct Block {
    pub stmts: Vec<Id<Stmt>>,
    pub ty: Id<Type>,
    pub span: Option<Span>,
}
