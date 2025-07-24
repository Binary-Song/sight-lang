use crate::ast::span::*;
use crate::ast::typed::Expr;
use crate::{
    ast::typed::{
        binding::Binding,
        expr::{Block, ExprSumId},
        pattern::PatternSumId,
        ty::Type,
    },
    container::*,
};
use sight_macros::{make_sum_id, LiteralValue};

make_sum_id!( 
    target_type: Stmt,
    id_type: StmtSumId,
    LetStmt: LetStmt,
    FunctionStmt: FunctionStmt,
    EmptyStmt: EmptyStmt,
);

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct LetStmt {
    pub binding: Id<Binding>,
    pub lhs: Id<String>,
    pub rhs: Id<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct FunctionStmt {
    pub binding: Id<Binding>,
    pub param: PatternSumId,
    pub ret_ty: Id<Type>,
    pub body: Id<Block>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct EmptyStmt {
    pub span: Option<Span>,
}
