use crate::ast::span::*;
use crate::ast::typed::binding::Binding;
use crate::ast::typed::ty::{PrimitiveType, TupleType, Type};
use crate::ast::typed::GetTy;
use crate::ast::typed::{IdStmt, Stmt};
use crate::container::*;
use sight_macros::{make_sum_id, Item, LiteralValue};
use std::marker::PhantomData;

make_sum_id!(
    target_type: Expr,
    id_type: IdExpr,
    LiteralExpr: LiteralExpr,
    VariableExpr: VariableExpr,
    ApplicationExpr: ApplicationExpr,
    BlockExpr: BlockExpr,
    TupleExpr: TupleExpr,
    ProjectionExpr: ProjectionExpr,
);

impl Expr {
    pub fn tuple(span: Option<Span>) -> Expr {
        TupleExpr {
            elems: vec![],
            span,
        }
        .into()
    }
}

impl GetTy for Expr {
    fn get_ty(&self, c: &mut impl Container) -> Id<Type> {
        match self {
            Expr::LiteralExpr(e) => e.get_ty(c),
            Expr::VariableExpr(e) => e.get_ty(c),
            Expr::ApplicationExpr(e) => e.get_ty(c),
            Expr::BlockExpr(e) => e.get_ty(c),
            Expr::TupleExpr(e) => e.get_ty(c),
            Expr::ProjectionExpr(e) => e.get_ty(c),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub enum Literal {
    Int(i32),
    Bool(bool),
}

impl GetTy for Literal {
    fn get_ty(&self, c: &mut impl Container) -> Id<Type> {
        match self {
            Literal::Int(_) => PrimitiveType::Int.to_type().encode_f(c),
            Literal::Bool(_) => PrimitiveType::Bool.to_type().encode_f(c),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
pub struct LiteralExpr {
    pub value: Literal,
    pub span: Option<Span>,
}

impl GetTy for LiteralExpr {
    fn get_ty(&self, c: &mut impl Container) -> Id<Type> {
        self.value.get_ty(c)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
pub struct VariableExpr {
    pub binding: Id<Binding>,
    pub name: Id<String>,
    pub ty: Id<Type>,
    pub span: Option<Span>,
}

impl GetTy for VariableExpr {
    fn get_ty(&self, c: &mut impl Container) -> Id<Type> {
        self.ty
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
pub struct ApplicationExpr {
    pub callee: IdExpr,
    pub arg: IdExpr,
    pub ty: Id<Type>,
    pub span: Option<Span>,
}

impl GetTy for ApplicationExpr {
    fn get_ty(&self, c: &mut impl Container) -> Id<Type> {
        self.ty
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
pub struct BlockExpr {
    pub block: Id<Block>,
}

impl GetTy for BlockExpr {
    fn get_ty(&self, c: &mut impl Container) -> Id<Type> {
        self.block.decode_f(c).get_ty(c)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
pub struct TupleExpr {
    pub elems: Vec<IdExpr>,
    pub span: Option<Span>,
}

impl GetTy for TupleExpr {
    fn get_ty(&self, c: &mut impl Container) -> Id<Type> {
        let tys = self
            .elems
            .iter()
            .map(|elem| elem.clone().decode_f(c).get_ty(c))
            .collect::<Vec<_>>();
        let tuple: Type = TupleType { elems: tys }.into();
        tuple.encode_f(c)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
pub struct ProjectionExpr {
    pub target: IdExpr,
    pub index: usize,
    pub span: Option<Span>,
    pub ty: Id<Type>,
}

impl GetTy for ProjectionExpr {
    fn get_ty(&self, c: &mut impl Container) -> Id<Type> {
        self.ty
    }
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue, Hash, Item)]
pub struct Block {
    pub stmts: Vec<IdStmt>,
    pub value: IdExpr,
    pub span: Option<Span>,
}

impl GetTy for Block {
    fn get_ty(&self, c: &mut impl Container) -> Id<Type> {
        self.value.decode_f(c).get_ty(c)
    }
}
