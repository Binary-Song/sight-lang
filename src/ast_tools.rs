use std::f32::consts::E;

use super::ast::*;

impl Expr {
    fn map<F: Fn(&Expr) -> Expr>(&self, f: F) -> Expr {
        match self {
            Expr::Var { .. } =>  self.clone(),
            Expr::IntLit { .. } => self.clone(),
            Expr::BoolLit { .. } => self.clone(),
            Expr::UnaryOp { op, arg, span } => Expr::UnaryOp {
                op: op.clone(),
                arg: Box::new(f(arg.as_ref())),
                span: span.clone(),
            },
            Expr::BinaryOp {
                op,
                arg1,
                arg2,
                span,
            } => Expr::BinaryOp {
                op: op.clone(),
                arg1: Box::new(f(arg1.as_ref())),
                arg2: Box::new(f(arg2.as_ref())),
                span: span.clone(),
            },
            Expr::App { func, args, span } => Expr::App {
                func: Box::new(f(func.as_ref())),
                args: args.clone(),
                span: span.clone(),
            },
            Expr::Func {
                params,
                body,
                ret_ty,
                span,
            } => Expr::Func {
                params: params.clone(),
                body: Box::new(f(body.as_ref())),
                ret_ty: ret_ty.clone(),
                span: span.clone(),
            },
            Expr::Let {
                name,
                ty,
                expr,
                body,
                span,
            } => Expr::Let {
                name: name.clone(),
                ty: ty.clone(),
                expr: Box::new(f(&expr.as_ref())),
                body: (),
                span: span.clone(),
            },
        }
    }

    fn is_value(&self) -> bool {
        match self {
            Expr::IntLit { .. } => true,
            Expr::BoolLit { .. } => true,
            Expr::UnaryOp { .. } => false,
            Expr::BinaryOp { .. } => false,
            Expr::App { .. } => false,
            Expr::Var { .. } => false,
            Expr::Func { .. } => true,
            Expr::Let { .. } => false,
        }
    }
}
