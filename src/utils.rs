use std::fmt::format;

use super::ast::*;
use crate::typing::*;

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
                init: expr,
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

impl TestPrintV1 for Term {
    fn print_v1(self: &Term) -> String {
        match self {
            Term::Literal {
                value: Literal::Unit,
                ..
            } => "<UnitLit/>".to_string(),
            Term::Literal {
                value: Literal::Bool(value),
                ..
            } => format!("<BoolLit value=\"{}\"/>", value),
            Term::Literal {
                value: Literal::Int(value),
                ..
            } => format!("<IntLit value=\"{}\"/>", value),
            Term::Var { name, span } => format!("<Var name=\"{}\"/>", name),
            Term::App { callee, args, .. } => {
                format!(
                    "<App><Func>{:?}</Func><Args>{}</Args></App>",
                    callee,
                    args.print_v1(),
                )
            }
            Term::Func {
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
            Term::Let {
                name,
                ty,
                rhs: expr,
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
            Term::Seq { seq, span } => {
                format!("<Seq>{}</Seq>", seq.print_v1())
            }
            Term::Op { op, span } => {
                format!("<Op op=\"{:?}\"/>", op)
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
            "<Binding><Name>{}</Name><Ty>{}</Ty></Binding>",
            self.name,
            self.ty.print_v1()
        )
    }
}

impl TestPrintV1 for Type {
    fn print_v1(self: &Self) -> String {
        match self {
            Type::PrimitiveType(primitive_type) => {
                match primitive_type {
                    PrimitiveType::Unit => "()".to_string(),
                    PrimitiveType::Int => "int".to_string(),
                    PrimitiveType::Bool => "bool".to_string(),
                }
            }
            Type::Function { lhs, rhs } => {
                format!("(({}) -> {})", lhs.print_v1(), rhs.print_v1())
            }
            Type::Variable { name } => {
                format!("{}", name)
            }
        }
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
            result.push_str(&format!("{},", item.print_v1()));
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
