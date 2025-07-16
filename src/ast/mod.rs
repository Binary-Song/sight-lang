pub mod display;
pub mod id;
pub mod typed;

use crate::{parser::exprs::Prec, span::Span, LiteralValue};
use sight_macros::LiteralValue;
use std::fmt::Debug;

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum UnaryOp {
    Pos,
    Neg,
}

impl UnaryOp {
    pub fn name(&self) -> String {
        match self {
            UnaryOp::Pos => "u+".to_string(),
            UnaryOp::Neg => "u-".to_string(),
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl BinaryOp {
    pub fn name(&self) -> String {
        match self {
            BinaryOp::Add => "+".to_string(),
            BinaryOp::Sub => "-".to_string(),
            BinaryOp::Mul => "*".to_string(),
            BinaryOp::Div => "/".to_string(),
        }
    }

    pub fn prec(&self) -> Prec {
        match self {
            BinaryOp::Add | BinaryOp::Sub => Prec::OpAddSub,
            BinaryOp::Mul | BinaryOp::Div => Prec::OpMulDiv,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Op {
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
}

impl Op {
    pub fn name(&self) -> String {
        match self {
            Op::UnaryOp(op) => op.name(),
            Op::BinaryOp(op) => op.name(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Lit {
    Unit,
    Int(i32),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Expr {
    Unit {
        span: Option<Span>,
    },
    Int {
        value: i32,
        span: Option<Span>,
    },
    Bool {
        value: bool,
        span: Option<Span>,
    },
    // a var reference
    Var {
        name: String,
        span: Option<Span>,
    },
    UnaryOp {
        op: UnaryOp,
        arg: Box<Self>,
        span: Option<Span>,
        op_span: Option<Span>,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Self>,
        rhs: Box<Self>,
        span: Option<Span>,
        op_span: Option<Span>,
    },
    App {
        func: Box<Self>,
        arg: Box<Self>,
        span: Option<Span>,
    },
    Tuple {
        elems: Vec<Self>,
        span: Option<Span>,
    },
    Block(Box<Block>),
}

/// Blocks are braced Stmts. A Block can double as an Expr
/// or a Stmt. Like Rust, if a Block ends in an Expr,
/// the latter will be considered the 'return value' of the Block.
/// If a Blocks ends in a non-Expr Stmt, a fake unit
/// Expr will be inserted to the end of the Block which will
/// function as the return value.
#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Option<Span>,
}

/// Stmts are what comprise of Blocks. A Stmt is not an Expr.
/// We have to introduce the concept of Stmts because
/// things like `let a = t` and `fn a(){...}` are not
/// valid Exprs.
#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Stmt {
    Let {
        lhs: Pattern,
        rhs: Expr,
        span: Option<Span>,
    },
    Func(Box<Func>),
    Block(Block),
    Expr {
        expr: Expr,
        span: Option<Span>,
    },
    Empty {
        span: Option<Span>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Pattern {
    Unit {
        span: Option<Span>,
    },
    Var {
        name: String,
        ty: Option<TypeExpr>,
        span: Option<Span>,
    },
    Tuple {
        elems: Vec<Self>,
        span: Option<Span>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct Func {
    pub name: String, // todo: change this to intern string
    pub param: Pattern,
    pub ret_ty: TypeExpr,
    pub body: Block,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum TypeExpr {
    Unit {
        span: Option<Span>,
    },
    Bool {
        span: Option<Span>,
    },
    Int {
        span: Option<Span>,
    },
    Arrow {
        lhs: Box<TypeExpr>,
        rhs: Box<TypeExpr>,
        span: Option<Span>,
    },
    Tuple {
        elems: Vec<TypeExpr>,
        span: Option<Span>,
    },
}

// pub trait AST {
//     fn accept<E, V: Visitor<E>>(&mut self, visitor: &V) -> Result<(), E>;
// }
