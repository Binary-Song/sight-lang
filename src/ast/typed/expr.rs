use crate::{ast::typed::*, sema::inference::Constraint, utils::interning::InternString};
use sight_macros::LiteralValue;

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Literal {
    Int(i32),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct LiteralExpr {
    pub value: Literal,
    pub span: (usize, usize),
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct VariableExpr {
    pub target: BindingId,
    pub name: InternString,
    pub ty: TypeId,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct ApplicationExpr {
    pub callee: ExprIdSum,
    pub arg: ExprIdSum,
    pub ty: TypeId,
    pub constraint: Id<Constraint>,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct BlockExpr {
    pub block: Id<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct TupleExpr {
    pub elems: Vec<ExprIdSum>,
    pub span: (usize, usize),
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct Block {
    pub stmts: Vec<StmtId>,
    pub ty: TypeId,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, LiteralValue)]
pub enum ExprIdSum {
    Literal(Id<LiteralExpr>),
    Variable(Id<VariableExpr>),
    Application(Id<ApplicationExpr>),
    Block(Id<BlockExpr>),
    Tuple(Id<TupleExpr>),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum ExprSum {
    Literal(LiteralExpr),
    Variable(VariableExpr),
    Application(ApplicationExpr),
    Block(BlockExpr),
    Tuple(TupleExpr),
}

impl ExprIdSum {
    pub fn deref(self, arena: &impl GetArena) -> ExprSum {
        match self {
            ExprIdSum::Literal(id) => ExprSum::Literal(id.de(arena) ),
            ExprIdSum::Variable(id) => ExprSum::Variable(id.de(arena)),
            ExprIdSum::Application(id) => ExprSum::Application(id.de(arena)),
            ExprIdSum::Block(id) => ExprSum::Block(id.de(arena)),
            ExprIdSum::Tuple(id) => ExprSum::Tuple(id.de(arena)),
        }
    }
}

impl ExprSum {
    pub fn ty(&self, arena: &impl GetArena) -> TypeId {
        match self {
            ExprSum::Literal(expr) => expr.ty,
            ExprSum::Variable(expr) => expr.ty,
            ExprSum::Application(expr) => expr.ty,
            ExprSum::Block(expr) => expr.block.de(arena).ty,
            ExprSum::Tuple(expr) => expr.ty,
        }
    }
}
