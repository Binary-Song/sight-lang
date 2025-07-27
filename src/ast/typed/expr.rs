use crate::ast::span::*;
use crate::ast::typed::binding::IdBinding;
use crate::ast::typed::ty::{PrimitiveType, TupleType, Type};
use crate::ast::typed::GetTy;
use crate::ast::typed::IdStmt;
use crate::container::*;
use sight_macros::{make_sum_id, Item, LiteralValue};

make_sum_id!(
    target_type: Expr,
    id_type: IdExpr,
    Lit: LitExpr,
    Var: VarExpr,
    App: AppExpr,
    Block: BlockExpr,
    Tuple: TupleExpr,
    Proj: ProjExpr,
);

impl Expr {
    pub fn unit(span: Option<Span>) -> Expr {
        TupleExpr {
            elems: vec![],
            span,
        }
        .upcast()
    }
}

impl GetTy for Expr {
    fn get_ty(&self, c: &mut impl Container) -> Id<Type> {
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
    fn get_ty(&self, c: &mut impl Container) -> Id<Type> {
        match self {
            Literal::Int(_) => PrimitiveType::Int.upcast().encode_f(c),
            Literal::Bool(_) => PrimitiveType::Bool.upcast().encode_f(c),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
pub struct LitExpr {
    pub value: Literal,
    pub span: Option<Span>,
}

impl GetTy for LitExpr {
    fn get_ty(&self, c: &mut impl Container) -> Id<Type> {
        self.value.get_ty(c)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
pub struct VarExpr {
    pub binding: IdBinding,
    pub name: Id<String>,
    pub ty: Id<Type>,
    pub span: Option<Span>,
}

impl GetTy for VarExpr {
    fn get_ty(&self, c: &mut impl Container) -> Id<Type> {
        self.ty
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
pub struct AppExpr {
    pub callee: IdExpr,
    pub args: Vec<IdExpr>,
    pub ty: Id<Type>,
    pub span: Option<Span>,
}

impl GetTy for AppExpr {
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
pub struct ProjExpr {
    pub tuple: IdExpr,
    pub index: usize,
    pub span: Option<Span>,
    pub ty: Id<Type>,
}

impl GetTy for ProjExpr {
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
