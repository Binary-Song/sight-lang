use crate::{
    ast::span::*,
    container::{Container, Id},
};
use std::marker::PhantomData;

pub trait HasTupleSyntax {
    /// If self is a tuple, break it into its elements.
    /// Otherwise, return self as an Err.
    fn break_tuple(self) -> Result<Vec<Self>, Self>
    where
        Self: Sized;

    /// Make tuple from a vector of Self
    fn make_tuple(v: Vec<Self>, span: Option<Span>) -> Self
    where
        Self: Sized;

    /// Make tuple from a vector of Self
    fn make_closed_tuple(v: Vec<Self>, span: Option<Span>) -> Self
    where
        Self: Sized;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lit<C: Container> {
    PhantomData(PhantomData<C>),
    Unit,
    Bool(bool),
    Int(i32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BasicType<C: Container> {
    PhantomData(PhantomData<C>),
    Unit,
    Bool,
    Int,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<C: Container> {
    PhantomData(PhantomData<C>),
    Lit {
        value: Lit<C>,
        span: Option<Span>,
    },
    Var {
        name: Id<String>,
        span: Option<Span>,
    },
    App {
        func: Box<Self>,
        args: Vec<Self>,
        span: Option<Span>,
    },
    Tuple {
        elems: Vec<Self>,
        span: Option<Span>,
    },
    ClosedTuple {
        elems: Vec<Self>,
        span: Option<Span>,
    },
    Proj {
        tuple: Box<Self>,
        index: usize,
        span: Option<Span>,
    },
    Block(Box<Block<C>>),
    Lambda {
        params: Vec<Param<C>>,
        body: Box<Self>,
        span: Option<Span>,
    },
}

impl<C: Container> Expr<C> {
    pub fn unit(span: Option<Span>) -> Self {
        Expr::Tuple {
            elems: vec![],
            span: span,
        }
    }
}

impl<C: Container> GetSpanRef for Expr<C> {
    fn get_span_ref(&self) -> &Option<Span> {
        match self {
            Expr::PhantomData(_) => &None,
            Expr::Lit { span, .. }
            | Expr::Var { span, .. }
            | Expr::App { span, .. }
            | Expr::Tuple { span, .. }
            | Expr::ClosedTuple { span, .. }
            | Expr::Lambda { span, .. }
            | Expr::Proj { span, .. } => span,
            Expr::Block(block) => &block.span,
        }
    }
}

impl<C: Container> GetSpanMut for Expr<C> {
    fn get_span_mut(&mut self) -> &mut Option<Span> {
        match self {
            Expr::PhantomData(_) => todo!("PhantomData should not be used"),
            Expr::Lit { span, .. }
            | Expr::Var { span, .. }
            | Expr::App { span, .. }
            | Expr::Tuple { span, .. }
            | Expr::ClosedTuple { span, .. }
            | Expr::Lambda { span, .. }
            | Expr::Proj { span, .. } => span,
            Expr::Block(block) => &mut block.span,
        }
    }
}

impl<C: Container> HasTupleSyntax for Expr<C> {
    fn break_tuple(self) -> Result<Vec<Self>, Self> {
        match self {
            Self::Tuple { elems, .. } => Ok(elems),
            x => Err(x),
        }
    }
    fn make_tuple(v: Vec<Self>, span: Option<Span>) -> Self {
        Self::Tuple {
            elems: v,
            span: span,
        }
    }
    fn make_closed_tuple(v: Vec<Self>, span: Option<Span>) -> Self {
        Self::ClosedTuple {
            elems: v,
            span: span,
        }
    }
}

/// Blocks are braced Stmts. A Block can double as an Expr
/// or a Stmt. Like Rust, if a Block ends in an Expr,
/// the latter will be considered the 'return value' of the Block.
/// If a Blocks ends in a non-Expr Stmt, a fake unit
/// Expr will be inserted to the end of the Block which will
/// function as the return value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block<C: Container> {
    pub stmts: Vec<Stmt<C>>,
    pub value: Option<Expr<C>>,
    pub span: Option<Span>,
}

/// Stmts are what comprise of Blocks. A Stmt is not an Expr.
/// We have to introduce the concept of Stmts because
/// things like `let a = t` and `fn a(){...}` are not
/// valid Exprs.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt<C: Container> {
    Let { lhs: Pattern<C>, rhs: Expr<C> },
    Func { func: Box<Func<C>> },
    Expr { expr: Expr<C> },
    Empty { span: Option<Span> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern<C: Container> {
    Lit {
        value: Lit<C>,
        span: Option<Span>,
    },
    Var {
        name: Id<String>,
        span: Option<Span>,
    },
    Tuple {
        elems: Vec<Self>,
        span: Option<Span>,
    },
    ClosedTuple {
        elems: Vec<Self>,
        span: Option<Span>,
    },
}

impl<C: Container> Pattern<C> {
    pub fn unit(span: Option<Span>) -> Self {
        Pattern::Tuple {
            elems: vec![],
            span: span,
        }
    }
}

impl<C: Container> GetSpanRef for Pattern<C> {
    fn get_span_ref(&self) -> &Option<Span> {
        match self {
            Pattern::Var { span, .. }
            | Pattern::Lit { span, .. }
            | Pattern::Tuple { span, .. }
            | Pattern::ClosedTuple { span, .. } => span,
        }
    }
}

impl<C: Container> GetSpanMut for Pattern<C> {
    fn get_span_mut(&mut self) -> &mut Option<Span> {
        match self {
            Pattern::Var { span, .. }
            | Pattern::Lit { span, .. }
            | Pattern::Tuple { span, .. }
            | Pattern::ClosedTuple { span, .. } => span,
        }
    }
}

impl<C: Container> HasTupleSyntax for Pattern<C> {
    fn break_tuple(self) -> Result<Vec<Self>, Self> {
        match self {
            Self::Tuple { elems, .. } => Ok(elems),
            x => Err(x),
        }
    }
    fn make_tuple(v: Vec<Self>, span: Option<Span>) -> Self {
        Self::Tuple {
            elems: v,
            span: span,
        }
    }
    fn make_closed_tuple(v: Vec<Self>, span: Option<Span>) -> Self {
        Self::ClosedTuple {
            elems: v,
            span: span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param<C: Container> {
    pub name: Id<String>,
    pub ty: Option<TypeExpr<C>>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func<C: Container> {
    pub name: Id<String>,
    pub params: Vec<Param<C>>,
    pub ret_ty: TypeExpr<C>,
    pub body: Block<C>,
    pub name_span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeExpr<C: Container> {
    Basic {
        t: BasicType<C>,
        span: Option<Span>,
    },
    Var {
        name: Id<String>,
        span: Option<Span>,
    },
    Arrow {
        lhs: Box<TypeExpr<C>>,
        rhs: Box<TypeExpr<C>>,
        span: Option<Span>,
    },
    Tuple {
        elems: Vec<TypeExpr<C>>,
        span: Option<Span>,
    },
    ClosedTuple {
        elems: Vec<Self>,
        span: Option<Span>,
    },
}

impl<C: Container> TypeExpr<C> {
    pub fn unit(span: Option<Span>) -> Self {
        TypeExpr::Tuple {
            elems: vec![],
            span: span,
        }
    }
}

impl<C: Container> GetSpanRef for TypeExpr<C> {
    fn get_span_ref(&self) -> &Option<Span> {
        match self {
            TypeExpr::Basic { span, .. }
            | TypeExpr::Var { span, .. }
            | TypeExpr::Arrow { span, .. }
            | TypeExpr::Tuple { span, .. }
            | TypeExpr::ClosedTuple { span, .. } => span,
        }
    }
}

impl<C: Container> GetSpanMut for TypeExpr<C> {
    fn get_span_mut(&mut self) -> &mut Option<Span> {
        match self {
            TypeExpr::Basic { span, .. }
            | TypeExpr::Var { span, .. }
            | TypeExpr::Arrow { span, .. }
            | TypeExpr::Tuple { span, .. }
            | TypeExpr::ClosedTuple { span, .. } => span,
        }
    }
}

impl<C: Container> HasTupleSyntax for TypeExpr<C> {
    fn break_tuple(self) -> Result<Vec<Self>, Self> {
        match self {
            Self::Tuple { elems, .. } => Ok(elems),
            x => Err(x),
        }
    }
    fn make_tuple(v: Vec<Self>, span: Option<Span>) -> Self {
        Self::Tuple {
            elems: v,
            span: span,
        }
    }
    fn make_closed_tuple(v: Vec<Self>, span: Option<Span>) -> Self {
        Self::ClosedTuple {
            elems: v,
            span: span,
        }
    }
}
