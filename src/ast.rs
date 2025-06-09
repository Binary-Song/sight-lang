#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Pos,
    Neg,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Mul,
    // sequencing op ";"
    Seq,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct L0ExprTag;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct L1ExprTag;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct L2ExprTag;

pub trait GExprTag<
    Mu, // yeah, the evil μ-type...
>
{
    // this can be either a String or a number
    type Name;
    // this is either () or a Box<Mu>
    type LetBody;
}

impl<Mu> GExprTag<Mu> for L0ExprTag {
    type Name = String;
    type LetBody = ();
}

impl<Mu> GExprTag<Mu> for L1ExprTag {
    type Name = String;
    type LetBody = Box<Mu>;
}

impl<Mu> GExprTag<Mu> for L2ExprTag {
    type Name = u32;
    type LetBody = Box<Mu>;
}

// named terms (the rawest form of the AST)
pub type Expr = GExpr<L0ExprTag>;

// after de-sugaring let.
pub type L1Expr = GExpr<L1ExprTag>;

// after removing names. (unnamed terms)
pub type L2Expr = GExpr<L2ExprTag>;

// the generic expression type
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GExpr<Tag: GExprTag<Self /* here, we plug in μ=Self */>> {
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
        name: Tag::Name,
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
        name: Tag::Name,
        ty: Option<Ty>,
        expr: Box<Self>,
        body: Tag::LetBody, // () or Box<Self>,
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
