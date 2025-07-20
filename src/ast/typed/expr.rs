use crate::ast::span::*;
use crate::container::*;
use sight_macros::LiteralValue;
use std::marker::PhantomData;

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Literal<'c, A: Arena<Self>> {
    Phantom(PhantomData<&'c ()>),
    Int(i32),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct LiteralExpr<'c, A: Arena<Self>> {
    pub value: Literal<'c, A>,
    pub span: Option<Span>,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct VariableExpr<'c, A: Arena<Self>> {
    pub target: Id<'c, Binding<'c, A>, A>,
    pub name: StringId<'c>,
    pub ty: TypeId,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct ApplicationExpr<'c, A: Arena<Self>> {
    pub callee: Id<'c, Expr<'c, A>, A>,
    pub arg: Id<'c, Expr<'c, A>, A>,
    pub ty: TypeId,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct BlockExpr<'c, A: Arena<Self>> {
    pub block: Id<'c, Block<'c, A>, A>,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct TupleExpr<'c, A: Arena<Self>> {
    pub elems: Vec<ExprId>,
    pub span: Option<Span>,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct ProjectionExpr<'c, A: Arena<Self>> {
    pub target: ExprId,
    pub index: usize,
    pub span: Option<Span>,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct Block<'c, A: Arena<Self>> {
    pub stmts: Vec<StmtId>,
    pub ty: TypeId,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, LiteralValue)]
pub enum ExprId<'c, A: Arena<Self>> {
    Literal(Id<LiteralExpr>),
    Variable(Id<VariableExpr>),
    Application(Id<ApplicationExpr>),
    Block(Id<BlockExpr>),
    Tuple(Id<TupleExpr>),
    Projection(Id<ProjectionExpr>),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Expr<'c, A: Arena<Self>> {
    Literal(LiteralExpr),
    Variable(VariableExpr),
    Application(ApplicationExpr),
    Block(BlockExpr),
    Tuple(TupleExpr),
    Projection(ProjectionExpr),
}

impl<'c, A: Arena<Self>> ExprId<'c, A> {
    pub fn de(self, arena: &impl GetArena) -> Expr {
        match self {
            ExprId::Literal(id) => Expr::Literal(id.de(arena)),
            ExprId::Variable(id) => Expr::Variable(id.de(arena)),
            ExprId::Application(id) => Expr::Application(id.de(arena)),
            ExprId::Block(id) => Expr::Block(id.de(arena)),
            ExprId::Tuple(id) => Expr::Tuple(id.de(arena)),
            ExprId::Projection(id) => Expr::Projection(id.de(arena)),
        }
    }
}

impl Expr {
    pub fn ty(&self, arena: &impl GetArena) -> TypeId {
        match self {
            Expr::Literal(expr) => expr.ty,
            Expr::Variable(expr) => expr.ty,
            Expr::Application(expr) => expr.ty,
            Expr::Block(expr) => expr.block.de(arena).ty,
            Expr::Tuple(expr) => expr.ty,
            Expr::Projection(expr) => expr.ty,
        }
    }
}
