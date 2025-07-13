use crate::{ast::typed::*, sema::inference::Constraint};
use sight_macros::LiteralValue;

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
            StmtId::Let(id) => Stmt::Let(id.de(arena)),
            StmtId::Function(id) => Stmt::Function(id.de(arena)),
            StmtId::Block(id) => Stmt::Block(id.de(arena)),
            StmtId::Expr(expr_id) => Stmt::Expr(expr_id),
            StmtId::Empty(id) => Stmt::Empty(id.de(arena)),
        }
    }
}
