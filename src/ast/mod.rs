pub mod display;
/// The typed ast. Differs from the untyped ast in that it has type info on
/// the nodes and is desugared a little bit.
pub mod typed;
pub mod visitor;

use crate::{ast::{typed::ScopeName, visitor::Visitor}, parser::exprs::Prec};
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
        span: (usize, usize),
    },
    Int {
        value: i32,
        span: (usize, usize),
    },
    Bool {
        value: bool,
        span: (usize, usize),
    },
    // a var reference
    Var {
        name: String,
        span: (usize, usize),
    },
    UnaryOp {
        op: UnaryOp,
        arg: Box<Self>,
        span: (usize, usize),
        op_span: (usize, usize),
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Self>,
        rhs: Box<Self>,
        span: (usize, usize),
        op_span: (usize, usize),
    },
    App {
        func: Box<Self>,
        arg: Box<Self>,
        span: (usize, usize),
    },
    Tuple {
        elems: Vec<Self>,
        span: (usize, usize),
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
    pub name: ScopeName,
    pub span: (usize, usize),
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
        span: (usize, usize),
    },
    Func(Box<Func>),
    Block(Block),
    Expr {
        expr: Expr,
        span: (usize, usize),
    },
    Empty {
        span: (usize, usize),
    },
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Pattern {
    Unit {
        span: (usize, usize),
    },
    Var {
        name: String,
        ty: Option<TypeExpr>,
        span: (usize, usize),
    },
    Tuple {
        elems: Vec<Self>,
        span: (usize, usize),
    },
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct Func {
    pub name: String,
    pub param: Pattern,
    pub ret_ty: TypeExpr,
    pub body: Expr,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum TypeExpr {
    Unit {
        span: (usize, usize),
    },
    Bool {
        span: (usize, usize),
    },
    Int {
        span: (usize, usize),
    },
    Arrow {
        lhs: Box<TypeExpr>,
        rhs: Box<TypeExpr>,
        span: (usize, usize),
    },
    Tuple {
        elems: Vec<TypeExpr>,
        span: (usize, usize),
    },
}

pub trait AST {
    fn accept<E, V: Visitor<E>>(&mut self, visitor: &V) -> Result<(), E>;
}
