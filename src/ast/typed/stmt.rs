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
pub enum StmtId {
    Let(Id<LetStmt>),
    Function(Id<FunctionStmt>),
    Block(Id<Block>),
    Expr(ExprIdSum),
    Empty(Id<EmptyStmt>),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Stmt {
    Let(LetStmt),
    Function(FunctionStmt),
    Block(Block),
    Expr(ExprIdSum),
    Empty(EmptyStmt),
}

impl StmtId {
    pub fn deref<T: GetArena>(self, arena: &T) -> Stmt {
        let arena = arena.get_arena();
        match self {
            StmtId::Let(id) => Stmt::Let(arena.deref(id).clone()),
            StmtId::Function(id) => Stmt::Function(arena.deref(id).clone()),
            StmtId::Block(id) => Stmt::Block(arena.deref(id).clone()),
            StmtId::Expr(expr_id) => Stmt::Expr(expr_id),
            StmtId::Empty(id) => Stmt::Empty(arena.deref(id).clone()),
        }
    }
}
