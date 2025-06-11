use std::rc::Rc;

use crate::{
    ast::{Op, TypeExpr, UnaryOp},
    LiteralValue,
};
use sight_macros::LiteralValue;

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Lit {
    Unit,
    Int(i32),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct Func {
    pub name: String,
    pub param: Pattern,
    pub ret_ty: Type,
    pub func_ty: Type,
    pub body: Expr,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Pattern {
    Unit {
        span: (usize, usize),
    },
    Var {
        name: String,
        ty: Type,
        span: (usize, usize),
    },
    Tuple {
        elems: Vec<Self>,
        ty: Type,
        span: (usize, usize),
    },
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Expr {
    Lit {
        value: Lit,
        span: (usize, usize),
    },
    // a var reference
    Var {
        name: String,
        span: (usize, usize),
        ty: Type,
    },
    Application {
        callee: Box<Expr>,
        arg: Box<Expr>,
        ty: Type,
        span: (usize, usize),
    },
    Let {
        lhs: Pattern,
        rhs: Box<Self>,
        body: Box<Self>,
        span: (usize, usize),
    },
    Seq {
        seq: Vec<Self>,
        ty: Type,
        span: (usize, usize),
    },
    Tuple {
        elems: Vec<Self>,
        ty: Type,
        span: (usize, usize),
    },
    Func {
        func: Rc<Func>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Arrow { lhs: Box<Type>, rhs: Box<Type> },
    Tuple { elems: Vec<Type> },
    TypeVar { index: u32 },
}

/////////////////////////////////////////
/// Trait and impl

pub trait Typed {
    fn get_type(&self) -> Type;
}

impl Typed for Pattern {
    fn get_type(&self) -> Type {
        match self {
            Pattern::Unit { .. } => Type::Unit,
            Pattern::Var { ty, .. } | Pattern::Tuple { ty, .. } => ty.clone(),
        }
    }
}

impl Typed for Lit {
    fn get_type(&self) -> Type {
        match self {
            Lit::Unit => Type::Tuple { elems: vec![] },
            Lit::Int(_) => Type::Int,
            Lit::Bool(_) => Type::Bool,
        }
    }
}

impl Typed for UnaryOp {
    fn get_type(&self) -> Type {
        match self {
            UnaryOp::Neg => Type::Int,
            UnaryOp::Pos => Type::Int,
        }
    }
}

impl Typed for Expr {
    fn get_type(&self) -> Type {
        match self {
            Expr::Lit { value, .. } => value.get_type(),
            Expr::Var { ty, .. }
            | Expr::Application { ty, .. }
            | Expr::Seq { ty, .. }
            | Expr::Tuple { ty, .. } => ty.clone(),
            Expr::Let { .. } | Expr::Func { .. } => Type::Unit,
        }
    }
}
