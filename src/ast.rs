#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltInOp {
    Add,
    Mul,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    IntLit {
        value: i32,
        span: (usize, usize),
    },
    BoolLit {
        value: bool,
        span: (usize, usize),
    },
    Var {
        name: String,
        span: (usize, usize),
    },
    Abs {
        name: String,
        ty: Ty,
        body: Box<Expr>,
        span: (usize, usize),
    },
    App {
        func: Box<Expr>,
        arg: Box<Expr>,
        span: (usize, usize),
    },
    // As{func: Box<Expr>, span: (usize, usize)},
    // todo: generate 'as' on built-in ops to skip having to give typing rules to each built-inop
    BuiltInOp {
        op: BuiltInOp,
        l: Box<Expr>,
        r: Option<Box<Expr>>,
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

impl Expr {
    fn map<F: Fn(&Expr) -> Expr>(&self, f: F) -> Expr {
        match self {
            Expr::IntLit { .. } => self.clone(),
            Expr::BoolLit { .. } => self.clone(),
            Expr::Var { .. } => self.clone(),
            Expr::Abs {
                name,
                ty,
                body,
                span,
            } => Expr::Abs {
                body: Box::new(f(body)),
                name: name.clone(),
                ty: ty.clone(),
                span: *span,
            },
            Expr::App { func, arg, span } => Expr::App {
                func: Box::new(f(func)),
                arg: Box::new(f(arg)),
                span: *span,
            },
            Expr::BuiltInOp {
                op,
                l,
                r: Some(r),
                span,
            } => Expr::BuiltInOp {
                op: op.clone(),
                l: Box::new(f(l)),
                r: Some(Box::new(f(r))),
                span: *span,
            },
            Expr::BuiltInOp {
                op,
                l,
                r: None,
                span,
            } => Expr::BuiltInOp {
                op: op.clone(),
                l: Box::new(f(l)),
                r: None,
                span: *span,
            },
        }
    }

    fn is_value(&self) -> bool {
        match self {
            Expr::IntLit { .. } => true,
            Expr::BoolLit { .. } => true,
            Expr::Var { .. } => false,
            Expr::Abs { .. } => false,
            Expr::App { .. } => false,
            Expr::BuiltInOp { .. } => false,
        }
    }
}
