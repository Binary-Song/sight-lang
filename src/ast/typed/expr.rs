use crate::ast::span::*;
use crate::ast::typed::binding::IdBinding;
use crate::ast::typed::ty::{PrimitiveType, TupleType, Type};
use crate::ast::typed::GetTy;
use crate::ast::typed::IdStmt;
use crate::container::*;
use sight_macros::{IdEnum, Item, LiteralValue, Upcast};

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, IdEnum, Upcast)]
pub enum Expr {
    Lit(LitExpr),
    Var(VarExpr),
    App(AppExpr),
    Block(BlockExpr),
    Tuple(TupleExpr),
    Proj(ProjExpr),
}

impl Expr {
    pub fn unit(span: Option<Span>) -> Expr {
        TupleExpr {
            elems: vec![],
            span,
        }
        .to_expr()
    }
}

impl GetTy for Expr {
    fn get_ty(&self, c: &mut Container) -> Uid<Type> {
        match self {
            Expr::Lit(e) => e.get_ty(c),
            Expr::Var(e) => e.get_ty(c),
            Expr::App(e) => e.get_ty(c),
            Expr::Block(e) => e.get_ty(c),
            Expr::Tuple(e) => e.get_ty(c),
            Expr::Proj(e) => e.get_ty(c),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub enum Literal {
    Int(i32),
    Bool(bool),
}

impl GetTy for Literal {
    fn get_ty(&self, c: &mut Container) -> Uid<Type> {
        match self {
            Literal::Int(_) => PrimitiveType::Int.to_type().int(c),
            Literal::Bool(_) => PrimitiveType::Bool.to_type().int(c),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct LitExpr {
    pub value: Literal,
    pub span: Option<Span>,
}

impl GetTy for LitExpr {
    fn get_ty(&self, c: &mut Container) -> Uid<Type> {
        self.value.get_ty(c)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct VarExpr {
    pub binding: IdBinding,
    pub name: Uid<String>,
    pub ty: Uid<Type>,
    pub span: Option<Span>,
}

impl GetTy for VarExpr {
    fn get_ty(&self, _c: &mut Container) -> Uid<Type> {
        self.ty.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct AppExpr {
    pub callee: IdExpr,
    pub args: Vec<IdExpr>,
    pub ty: Uid<Type>,
    pub span: Option<Span>,
}

impl GetTy for AppExpr {
    fn get_ty(&self, _c: &mut Container) -> Uid<Type> {
        self.ty.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct BlockExpr {
    pub block: Id<Block>,
}

impl GetTy for BlockExpr {
    fn get_ty(&self, c: &mut Container) -> Uid<Type> {
        self.block.dec(c).get_ty(c)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct TupleExpr {
    pub elems: Vec<IdExpr>,
    pub span: Option<Span>,
}

impl GetTy for TupleExpr {
    fn get_ty(&self, c: &mut Container) -> Uid<Type> {
        let tys = self
            .elems
            .iter()
            .map(|elem| elem.clone().dec(c).get_ty(c))
            .collect::<Vec<_>>();
        let tuple = TupleType { elems: tys }.to_type();
        tuple.int(c)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct ProjExpr {
    pub tuple: IdExpr,
    pub index: usize,
    pub span: Option<Span>,
    pub ty: Uid<Type>,
}

impl GetTy for ProjExpr {
    fn get_ty(&self, _c: &mut Container) -> Uid<Type> {
        self.ty.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue, Hash)]
pub struct Block {
    pub stmts: Vec<IdStmt>,
    pub value: IdExpr,
    pub span: Option<Span>,
}

impl GetTy for Block {
    fn get_ty(&self, c: &mut Container) -> Uid<Type> {
        self.value.dec(c).get_ty(c)
    }
}
