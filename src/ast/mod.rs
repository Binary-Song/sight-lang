use sight_macros::LiteralValue;
use std::fmt::Debug;

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum UnaryOp {
    Pos,
    Neg,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    // Binary-Seq
    Seq,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Op {
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
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
        arg: Box<Self>,
        span: (usize, usize),
    },
    Tuple {
        elems: Vec<Self>,
        span: (usize, usize),
    },
    Block(Box<Block>),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub trailing_expr: Option<Expr>,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Stmt {
    Let {
        lhs: Pattern,
        rhs: Expr,
        span: (usize, usize),
    },
    Fn {
        name: String,
        param_pattern: Pattern,
        return_type: TypeExpr,
        body: Expr,
        span: (usize, usize),
    },
    Block(Block),
    Expr{ expr: Expr, span: (usize, usize)},
    Empty{ span: (usize, usize)},
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Pattern {
    Var {
        name: String,
        type_anno: TypeExpr,
        span: (usize, usize),
    },
    Tuple {
        elems: Vec<Self>,
        span: (usize, usize),
    },
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
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
        arg: Box<Term>,
        span: (usize, usize),
    },
    Func {
        param: Box<Binding>,
        ret_ty: Option<TypeExpr>,
        body: Box<Self>,
        span: (usize, usize),
    },
    Let {
        name: String,
        ty: Option<TypeExpr>,
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
    Tuple {
        elems: Vec<Self>,
        span: (usize, usize),
    },
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum TypeExpr {
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

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct Binding {
    pub name: String,
    pub ty: Option<TypeExpr>,
    pub span: (usize, usize),
}
