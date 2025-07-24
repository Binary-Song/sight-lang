use crate::ast::span::*;
use crate::ast::typed::Expr;
use crate::{
    ast::typed::{
        binding::Binding,
        expr::{Block, IdExpr},
        ty::Type,
    },
    container::*,
};
use sight_macros::{make_sum_id, Item, LiteralValue};

make_sum_id!(
    target_type: Stmt,
    id_type: IdStmt,
    LetStmt: LetStmt,
    FunctionStmt: FunctionStmt,
    EmptyStmt: EmptyStmt,
);

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
pub struct LetStmt {
    pub binding: Id<Binding>,
    pub lhs: Id<String>,
    pub rhs: IdExpr,
}

#[derive(Debug, Clone, PartialEq, LiteralValue, Eq, Hash, Item)]
pub struct Param {
    pub name: Id<String>,
    pub ty: Id<Type>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
pub struct FunctionStmt {
    pub binding: Id<Binding>,
    pub param: Id<Param>,
    pub ret_ty: Id<Type>,
    pub body: Id<Block>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
pub struct EmptyStmt {
    pub span: Option<Span>,
}
