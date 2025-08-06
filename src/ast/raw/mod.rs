use sight_macros::{Item, LiteralValue};

use crate::{
    ast::span::*,
    container::{Container, Id, Item},
    LiteralValue,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub enum Lit {
    Unit,
    Bool(bool),
    Int(i32),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub enum BasicType {
    Unit,
    Bool,
    Int,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub enum Expr {
    Lit {
        value: Lit,
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
    Block(Box<Block>),
}

impl Expr {
    pub fn unit(span: Option<Span>) -> Self {
        Expr::Tuple {
            elems: vec![],
            span: span,
        }
    }
}

impl GetSpanRef for Expr {
    fn get_span_ref(&self) -> &Option<Span> {
        match self {
            Expr::Lit { span, .. }
            | Expr::Var { span, .. }
            | Expr::App { span, .. }
            | Expr::Tuple { span, .. }
            | Expr::ClosedTuple { span, .. }
            | Expr::Proj { span, .. } => span,
            Expr::Block(block) => &block.span,
        }
    }
}

impl GetSpanMut for Expr {
    fn get_span_mut(&mut self) -> &mut Option<Span> {
        match self {
            Expr::Lit { span, .. }
            | Expr::Var { span, .. }
            | Expr::App { span, .. }
            | Expr::Tuple { span, .. }
            | Expr::ClosedTuple { span, .. }
            | Expr::Proj { span, .. } => span,
            Expr::Block(block) => &mut block.span,
        }
    }
}

impl HasTupleSyntax for Expr {
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub value: Option<Expr>,
    pub span: Option<Span>,
}

/// Stmts are what comprise of Blocks. A Stmt is not an Expr.
/// We have to introduce the concept of Stmts because
/// things like `let a = t` and `fn a(){...}` are not
/// valid Exprs.
#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub enum Stmt {
    Let {
        lhs: Id<String>,
        ty_ann: Option<TypeExpr>,
        rhs: Expr,
        name_span: Option<Span>,
    },
    Func {
        func: Box<Func>,
    },
    Expr {
        expr: Expr,
    },
    Empty {
        span: Option<Span>,
    },
    If {
        cond: Expr,
        then_br: Block,
        else_br: Block,
    },
    While {
        cond: Expr,
        body: Block,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub enum Pattern {
    Lit {
        value: Lit,
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

impl Pattern {
    pub fn unit(span: Option<Span>) -> Self {
        Pattern::Tuple {
            elems: vec![],
            span: span,
        }
    }
}

impl GetSpanRef for Pattern {
    fn get_span_ref(&self) -> &Option<Span> {
        match self {
            Pattern::Var { span, .. }
            | Pattern::Lit { span, .. }
            | Pattern::Tuple { span, .. }
            | Pattern::ClosedTuple { span, .. } => span,
        }
    }
}

impl GetSpanMut for Pattern {
    fn get_span_mut(&mut self) -> &mut Option<Span> {
        match self {
            Pattern::Var { span, .. }
            | Pattern::Lit { span, .. }
            | Pattern::Tuple { span, .. }
            | Pattern::ClosedTuple { span, .. } => span,
        }
    }
}

impl HasTupleSyntax for Pattern {
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

#[derive(Debug, Clone, PartialEq, LiteralValue, Eq, Hash)]
pub struct Param {
    pub name: Id<String>,
    pub ty_ann: TypeExpr,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct Func {
    pub name: Id<String>,
    pub params: Vec<Param>,
    pub ret_ty_ann: TypeExpr,
    pub body: Block,
    pub name_span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub enum TypeExpr {
    Basic {
        t: BasicType,
        span: Option<Span>,
    },
    Var {
        name: Id<String>,
        span: Option<Span>,
    },
    Arrow {
        lhs: Vec<TypeExpr>,
        rhs: Box<TypeExpr>,
        span: Option<Span>,
    },
    Tuple {
        elems: Vec<TypeExpr>,
        span: Option<Span>,
    },
    ClosedTuple {
        elems: Vec<Self>,
        span: Option<Span>,
    },
}

impl TypeExpr {
    pub fn unit(span: Option<Span>) -> Self {
        TypeExpr::Tuple {
            elems: vec![],
            span: span,
        }
    }
}

impl GetSpanRef for TypeExpr {
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

impl GetSpanMut for TypeExpr {
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

impl HasTupleSyntax for TypeExpr {
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
