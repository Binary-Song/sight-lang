use crate::ast::typed::*;
use sight_macros::{Internable, LiteralValue, StaticInternable};
use std::{collections::HashMap, hash::Hash};

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct LetStmt {
    pub lhs: PatternId,
    pub rhs: ExprIdSum,
    pub constraint: Id<Constraint>,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct FunctionStmt {
    pub new_fn_id: BindingId,
    pub param: PatternId,
    pub ret_ty: TypeId,
    pub body: Id<Block>,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct EmptyStmt {
    pub span: (usize, usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, LiteralValue)]
pub enum StmtIdSum {
    Let(Id<LetStmt>),
    Function(Id<FunctionStmt>),
    Block(Id<Block>),
    Expr(ExprIdSum),
    Empty(Id<EmptyStmt>),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum StmtSum {
    Let(LetStmt),
    Function(FunctionStmt),
    Block(Block),
    Expr(ExprIdSum),
    Empty(EmptyStmt),
}

impl StmtIdSum {
    pub fn deref(self, arena: &Arena) -> StmtSum {
        match self {
            StmtIdSum::Let(id) => StmtSum::Let(arena.deref(id).clone()),
            StmtIdSum::Function(id) => StmtSum::Function(arena.deref(id).clone()),
            StmtIdSum::Block(id) => StmtSum::Block(arena.deref(id).clone()),
            StmtIdSum::Expr(expr_id) => StmtSum::Expr(expr_id),
            StmtIdSum::Empty(id) => StmtSum::Empty(arena.deref(id).clone()),
        }
    }
}
