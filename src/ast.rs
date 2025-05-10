use std::fmt::Debug;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Pos,
    Neg,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Mul,
    // Binary-Seq
    Seq,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct L0ExprTag;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct L1ExprTag;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct L2ExprTag;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    UnitLit {
        span: (usize, usize),
    },
    IntLit {
        value: i32,
        span: (usize, usize),
    },
    BoolLit {
        value: bool,
        span: (usize, usize),
    },
    // a var reference
    Var {
        name: String,
        span: (usize, usize),
    },
    // todo: support for overloading, when args have MUTUALLY EXCLUSIVE types
    UnaryOp {
        op: UnaryOp,
        arg: Box<Self>,
        span: (usize, usize),
    },
    BinaryOp {
        op: BinaryOp,
        arg1: Box<Self>,
        arg2: Box<Self>,
        span: (usize, usize),
    },
    App {
        func: Box<Self>,
        args: Vec<Self>,
        span: (usize, usize),
    },
    Func {
        params: Vec<Binding>,
        ret_ty: Option<Ty>,
        body: Box<Self>,
        span: (usize, usize),
    },
    Let {
        name: String,
        ty: Option<Ty>,
        expr: Box<Self>,
        body: (),
        span: (usize, usize),
    },
    // Linear-Seq
    Seq {
        seq: Vec<Self>,
        span: (usize, usize),
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum L1Expr {
    UnitLit {
        span: (usize, usize),
    },
    IntLit {
        value: i32,
        span: (usize, usize),
    },
    BoolLit {
        value: bool,
        span: (usize, usize),
    },
    // a var reference
    Var {
        name: String,
        span: (usize, usize),
    },
    // todo: support for overloading, when args have MUTUALLY EXCLUSIVE types
    UnaryOp {
        op: UnaryOp,
        arg: Box<Self>,
        span: (usize, usize),
    },
    BinaryOp {
        op: BinaryOp,
        arg1: Box<Self>,
        arg2: Box<Self>,
        span: (usize, usize),
    },
    App {
        func: Box<Self>,
        args: Vec<Self>,
        span: (usize, usize),
    },
    Func {
        params: Vec<Binding>,
        ret_ty: Option<Ty>,
        body: Box<Self>,
        span: (usize, usize),
    },
    Let {
        name: String,
        ty: Option<Ty>,
        expr: Box<Self>,
        body: Box<Self>,
        span: (usize, usize),
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Bool {
        span: (usize, usize),
    },
    Int {
        span: (usize, usize),
    },
    Arrow {
        l: Box<Ty>,
        r: Box<Ty>,
        span: (usize, usize),
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding {
    pub name: String,
    pub ty: Option<Ty>,
    pub span: (usize, usize),
}
