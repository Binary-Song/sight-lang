use crate::ast::typed as t;
use crate::ast::desugared as d;


impl t::Func {
    fn desugar(&self) -> d::Func {
        d::Func {
            name: self.name.clone(),
            param: (self.param. ),
            ret_ty: todo!(),
            func_ty: todo!(),
            body: todo!(),
            span: todo!(),
        }
    }
}

impl t::Expr {
    fn desugar(&self) -> d::Expr {
        match self {
            t::Expr::Lit { value, span } => {
                d::Expr::Lit {
                    value: value.clone(),
                    span: *span,
                }
            },
            t::Expr::Var { name, span, ty , path } => {
                d::Expr::Var {
                    name: name.clone(),
                    span: *span,
                    ty: ty.clone(),
                    path: path.clone(),
                }
            },
            t::Expr::App { callee, arg, ty , cons, span } => {
                d::Expr::App {
                    callee: Box::new(callee.desugar()),
                    arg: Box::new(arg.desugar()),
                    ty: ty.clone(),
                    span: *span,
                }
            },
            t::Expr::Let { lhs, rhs, body, span, cons } => {
                d::Expr::Let {
                    lhs: lhs.clone(),
                    rhs: Box::new(rhs.desugar()),
                    body: Box::new(body.desugar()),
                    cons: cons.clone(),
                    span: *span,
                }
            },
            t::Expr::Seq { seq, ty, span } => {
                d::Expr::Seq {
                    seq: seq.iter().map(|e| e.desugar()).collect(),
                    ty: ty.clone(),
                    span: *span,
                }
            },
            t::Expr::Tuple { elems, ty, span } => {
                d::Expr::Tuple {
                    elems: elems.iter().map(|e| e.desugar()).collect(),
                    span: *span,  ty: ty.clone(),
                }
            },
            t::Expr::Func { func } => {
                d::Expr::Func {
                    func: Box::new(func.desugar()),
                }
            },
        }
    }
}