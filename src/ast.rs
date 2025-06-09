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
pub enum Op {
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lit {
    Unit,
    Int(i32),
    Bool(bool),
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
        rhs: Box<Self>,
        span: (usize, usize),
    },
    Seq {
        seq: Vec<Self>,
        span: (usize, usize),
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Lit {
        value: Lit,
        span: (usize, usize),
    },
    // a var reference
    Var {
        name: String,
        span: (usize, usize),
    },
    App {
        callee: Box<Term>,
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
        rhs: Box<Self>,
        body: Box<Self>,
        span: (usize, usize),
    },
    Seq {
        seq: Vec<Self>,
        span: (usize, usize),
    },
    Op {
        op: Op,
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
