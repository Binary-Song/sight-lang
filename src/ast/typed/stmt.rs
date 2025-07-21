use crate::ast::span::*;
use crate::{
    ast::typed::{
        binding::Binding,
        expr::{Block, ExprSumId},
        pattern::PatternSumId,
        r#type::Type,
    },
    container::*,
    sum_id,
};
use sight_macros::LiteralValue;

pub type StmtSumId = sum_id!(LetStmt, FunctionStmt, EmptyStmt);

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct LetStmt {
    pub lhs: PatternSumId,
    pub rhs: ExprSumId,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct FunctionStmt {
    pub new_fn_id: Id<Binding>,
    pub param: PatternSumId,
    pub ret_ty: Id<Type>,
    pub body: Id<Block>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct EmptyStmt {
    pub span: Option<Span>,
}
