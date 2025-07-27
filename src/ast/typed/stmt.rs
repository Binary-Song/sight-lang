use crate::ast::span::*;
use crate::ast::typed::Expr;
use crate::{
    ast::typed::{
        binding::*,
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
    ExprStmt: ExprStmt,
    EmptyStmt: EmptyStmt,
);

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
pub struct LetStmt {
    pub binding: Id<VarBinding>,
    pub name_span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
pub struct FunctionStmt {
    pub binding: Id<FuncBinding>,
    pub name_span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
pub struct EmptyStmt {
    pub span: Option<Span>,
}
