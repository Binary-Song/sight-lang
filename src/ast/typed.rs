use crate::{ast::UnaryOp, parser::context::ConstraintHandle};
use sight_macros::LiteralValue;

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Lit {
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

impl Pattern {
    pub fn unit(span: (usize, usize)) -> Self {
        Self::Tuple {
            elems: vec![],
            ty: Type::unit(),
            span: span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue, )]
pub enum ScopeName {
    Name(String),
    Index(usize),
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
        cons: ConstraintHandle,
        span: (usize, usize),
    },
    Let {
        lhs: Pattern,
        rhs: Box<Self>,
        body: Box<Self>,
        span: (usize, usize),
        cons: ConstraintHandle,
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
        func: Box<Func>,
    },
}

impl Expr {
    pub fn unit(span: (usize, usize)) -> Self {
        Self::Tuple {
            elems: vec![],
            ty: Type::unit(),
            span: span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Type {
    Bool,
    Int,
    Arrow { lhs: Box<Type>, rhs: Box<Type> },
    Tuple { elems: Vec<Type> },
    TypeVar { index: u32 },
}

impl Type {
    pub fn unit() -> Self {
        Type::Tuple { elems: vec![] }
    }
}

/////////////////////////////////////////
/// Trait and impl

pub trait Typed {
    /// Returns the type of the node.
    fn ty(&self) -> Type;
    /// Returns a Type if the node actually stores its own type (as opposed to deducing its type when queried).
    /// Otherwise, returns None.
    fn mut_ty(&mut self) -> Option<&mut Type>;
}

impl Typed for Pattern {
    fn ty(&self) -> Type {
        match self {
            Pattern::Var { ty, .. } | Pattern::Tuple { ty, .. } => ty.clone(),
        }
    }
    fn mut_ty(&mut self) -> Option<&mut Type> {
        match self {
            Pattern::Var { ty, .. } | Pattern::Tuple { ty, .. } => Some(ty),
        }
    }
}

impl Typed for Lit {
    fn ty(&self) -> Type {
        match self {
            Lit::Int(_) => Type::Int,
            Lit::Bool(_) => Type::Bool,
        }
    }
    fn mut_ty(&mut self) -> Option<&mut Type> {
        None // Literals do not have mutable types
    }
}

impl Typed for UnaryOp {
    fn ty(&self) -> Type {
        match self {
            UnaryOp::Neg => Type::Int,
            UnaryOp::Pos => Type::Int,
        }
    }
    fn mut_ty(&mut self) -> Option<&mut Type> {
        None // Unary operations do not have mutable types
    }
}

impl Typed for Expr {
    fn ty(&self) -> Type {
        match self {
            Expr::Lit { value, .. } => value.ty(),
            Expr::Var { ty, .. }
            | Expr::Application { ty, .. }
            | Expr::Seq { ty, .. }
            | Expr::Tuple { ty, .. } => ty.clone(),
            Expr::Let { .. } | Expr::Func { .. } => Type::unit(),
        }
    }
    fn mut_ty(&mut self) -> Option<&mut Type> {
        match self {
            Expr::Lit { .. } => None,
            Expr::Var { ty, .. }
            | Expr::Application { ty, .. }
            | Expr::Seq { ty, .. }
            | Expr::Tuple { ty, .. } => Some(ty),
            Expr::Let { .. } | Expr::Func { .. } => None,
        }
    }
}
