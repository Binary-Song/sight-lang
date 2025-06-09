use super::ast::*;

/// used for unit testing, designed to not break easily
/// when the AST changes
pub trait TestPrintV1 {
    fn print_v1(self: &Self) -> String;
}

impl Expr {
    pub fn span(self: &Expr) -> (usize, usize) {
        match self {
            Expr::UnitLit { span } => *span,
            Expr::IntLit { span, .. } => *span,
            Expr::BoolLit { span, .. } => *span,
            Expr::Var { span, .. } => *span,
            Expr::UnaryOp { span, .. } => *span,
            Expr::BinaryOp { span, .. } => *span,
            Expr::Seq { span, .. } => *span,
            Expr::App { span, .. } => *span,
            Expr::Func { span, .. } => *span,
            Expr::Let { span, .. } => *span,
        }
    }
}
pub fn print_vec_str_v1(vec: &Vec<String>) -> String {
    let mut result = String::new();
    for (i, s) in vec.iter().enumerate() {
        result.push_str(&format!("<Item{}>{}</Item{}>", i, s, i));
    }
    result
}

impl TestPrintV1 for Expr {
    fn print_v1(self: &Expr) -> String {
        match self {
            Expr::UnitLit { .. } => "<UnitLit/>".to_string(),
            Expr::IntLit { value, .. } => format!("<IntLit value=\"{}\"/>", value),
            Expr::BoolLit { value, .. } => format!("<BoolLit value=\"{}\"/>", value),
            Expr::Var { name, .. } => format!("<Var name=\"{}\"/>", name),
            Expr::UnaryOp { op, arg, .. } => {
                format!("<UnaryOp op=\"{:?}\">{}</UnaryOp>", op, arg.print_v1())
            }
            Expr::BinaryOp { op, arg1, arg2, .. } => {
                format!(
                    "<BinaryOp op=\"{:?}\"><Arg1>{}</Arg1><Arg2>{}</Arg2></BinaryOp>",
                    op,
                    arg1.print_v1(),
                    arg2.print_v1()
                )
            }
            Expr::App { func, args, .. } => {
                format!(
                    "<App><Func>{}</Func><Args>{}</Args></App>",
                    func.print_v1(),
                    args.print_v1(),
                )
            }
            Expr::Func {
                params,
                ret_ty,
                body,
                ..
            } => {
                format!(
                    "<Func><Params>{}</Params><RetTy>{}</RetTy><Body>{}</Body></Func>",
                    params.print_v1(),
                    ret_ty.print_v1(),
                    body.print_v1()
                )
            }
            Expr::Let {
                name,
                ty,
                expr,
                body,
                span,
            } => {
                format!(
                    "<Let><Name>{}</Name><Ty>{}</Ty><Expr>{}</Expr></Let>",
                    name,
                    ty.print_v1(),
                    expr.print_v1(),
                )
            }
            Expr::Seq { seq, span } => {
                format!("<Seq>{}</Seq>", seq.print_v1())
            }
        }
    }
}

impl TestPrintV1 for L1Expr {
    fn print_v1(self: &L1Expr) -> String {
        match self {
            L1Expr::UnitLit { .. } => "<UnitLit/>".to_string(),
            L1Expr::IntLit { value, .. } => format!("<IntLit value=\"{}\"/>", value),
            L1Expr::BoolLit { value, .. } => format!("<BoolLit value=\"{}\"/>", value),
            L1Expr::Var { name, .. } => format!("<Var name=\"{}\"/>", name),
            L1Expr::UnaryOp { op, arg, .. } => {
                format!("<UnaryOp op=\"{:?}\">{}</UnaryOp>", op, arg.print_v1())
            }
            L1Expr::BinaryOp { op, arg1, arg2, .. } => {
                format!(
                    "<BinaryOp op=\"{:?}\"><Arg1>{}</Arg1><Arg2>{}</Arg2></BinaryOp>",
                    op,
                    arg1.print_v1(),
                    arg2.print_v1()
                )
            }
            L1Expr::App { func, args, .. } => {
                format!(
                    "<App><Func>{}</Func><Args>{}</Args></App>",
                    func.print_v1(),
                    args.print_v1(),
                )
            }
            L1Expr::Func {
                params,
                ret_ty,
                body,
                ..
            } => {
                format!(
                    "<Func><Params>{}</Params><RetTy>{}</RetTy><Body>{}</Body></Func>",
                    params.print_v1(),
                    ret_ty.print_v1(),
                    body.print_v1()
                )
            }
            L1Expr::Let {
                name,
                ty,
                expr,
                body,
                span: _,
            } => {
                format!(
                    "<Let><Name>{}</Name><Ty>{}</Ty><Expr>{}</Expr><Body>{}</Body></Let>",
                    name,
                    ty.print_v1(),
                    expr.print_v1(),
                    body.print_v1()
                )
            }
        }
    }
}

impl TestPrintV1 for Ty {
    fn print_v1(self: &Ty) -> String {
        match self {
            Ty::Bool { span } => format!("<Bool/>"),
            Ty::Int { span } => format!("<Int/>"),
            Ty::Arrow { l, r, span } => {
                format!(
                    "<Arrow><L>{}</L><R>{}</R></Arrow>",
                    l.print_v1(),
                    r.print_v1()
                )
            }
        }
    }
}

impl TestPrintV1 for Binding {
    fn print_v1(self: &Binding) -> String {
        format!(
            "<Binding Name=\"{}\"><Ty>{}</Ty></Binding>",
            self.name,
            self.ty.print_v1()
        )
    }
}

// ==================== Utils ====================

impl<T> TestPrintV1 for Vec<T>
where
    T: TestPrintV1,
{
    fn print_v1(self: &Vec<T>) -> String {
        let mut result = String::new();
        for (i, item) in self.iter().enumerate() {
            let i = i + 1;
            result.push_str(&format!("<Item{}>{}</Item{}>", i, item.print_v1(), i));
        }
        result
    }
}

impl<T> TestPrintV1 for Box<T>
where
    T: TestPrintV1,
{
    fn print_v1(self: &Box<T>) -> String {
        self.as_ref().print_v1()
    }
}

impl<T> TestPrintV1 for Option<T>
where
    T: TestPrintV1,
{
    fn print_v1(self: &Option<T>) -> String {
        match self {
            Some(item) => item.print_v1(),
            None => "<None/>".to_string(),
        }
    }
}
