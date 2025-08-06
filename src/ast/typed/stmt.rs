use crate::{
    ast::typed::{
        binding::{FuncBinding, VarBinding},
    },
    container::*,
};
use sight_macros::{IdEnum, Item, LiteralValue};
use crate::ast::span::*;
use crate::ast::typed::Expr;

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, IdEnum)]
pub enum Stmt {
    LetStmt(LetStmt),
    FunctionStmt(FunctionStmt),
    ExprStmt(ExprStmt),
    EmptyStmt(EmptyStmt),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue,  )]
pub struct LetStmt {
    pub binding: Id<VarBinding>,
    pub name_span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue,  )]
pub struct FunctionStmt {
    pub binding: Id<FuncBinding>,
    pub name_span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue,  )]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue,  )]
pub struct EmptyStmt {
    pub span: Option<Span>,
}
