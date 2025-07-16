use crate::{ast::typed::*, sema::inference::Constraint, span::Span, utils::interning::InternString};
use sight_macros::LiteralValue;

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Literal {
    Int(i32),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct LiteralExpr {
    pub value: Literal,
    pub span: Option<Span>,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct VariableExpr {
    pub target: BindingId,
    pub name: InternString,
    pub ty: TypeId,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct ApplicationExpr {
    pub callee: ExprId,
    pub arg: ExprId,
    pub ty: TypeId,
    pub constraint: Id<Constraint>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct BlockExpr {
    pub block: Id<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct TupleExpr {
    pub elems: Vec<ExprId>,
    pub span: Option<Span>,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct ProjectionExpr {
    pub target: ExprId,
    pub index: usize,
    pub span: Option<Span>,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct Block {
    pub stmts: Vec<StmtId>,
    pub ty: TypeId,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, LiteralValue)]
pub enum ExprId {
    Literal(Id<LiteralExpr>),
    Variable(Id<VariableExpr>),
    Application(Id<ApplicationExpr>),
    Block(Id<BlockExpr>),
    Tuple(Id<TupleExpr>),
    Projection(Id<ProjectionExpr>),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Expr {
    Literal(LiteralExpr),
    Variable(VariableExpr),
    Application(ApplicationExpr),
    Block(BlockExpr),
    Tuple(TupleExpr),
    Projection(ProjectionExpr),
}

impl ExprId {
    pub fn de(self, arena: &impl GetArena) -> Expr {
        match self {
            ExprId::Literal(id) => Expr::Literal(id.de(arena) ),
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
