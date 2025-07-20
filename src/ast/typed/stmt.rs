use crate::container::*;
use sight_macros::LiteralValue;
use crate::ast::span::*;

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct LetStmt<'a, A: Arena<Self>> {
    pub lhs: PatternId,
    pub rhs: ExprId,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct FunctionStmt<'a, A: Arena<Self>> {
    pub new_fn_id: BindingId,
    pub param: PatternId,
    pub ret_ty: TypeId,
    pub body: Id<Block<'a, A>>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct EmptyStmt<'a, A: Arena<Self>> {
    pub span: Option<Span>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, LiteralValue)]
pub enum StmtId<'a, A: Arena<Self>> {
    Let(Id<LetStmt<'a, A>>),
    Function(Id<FunctionStmt<'a, A>>),
    Block(Id<Block<'a, A>>),
    Expr(ExprId),
    Empty(Id<EmptyStmt<'a, A>>),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Stmt<'a, A: Arena<Self>> {
    Let(LetStmt<'a, A>),
    Function(FunctionStmt<'a, A>),
    Block(Block<'a, A>),
    Expr(ExprId),
    Empty(EmptyStmt<'a, A>),
}

