lalrpop_mod!(pub syntax);
use std::{f64::consts::E, fmt::Binary};
pub use lalrpop_util::lalrpop_mod;
use crate::ast::*;

fn l1_desug(e: Expr) -> L1Expr {
    match e {
        Expr::UnitLit { span } => L1Expr::UnitLit { span },
        Expr::IntLit { value, span } => L1Expr::IntLit { value, span },
        Expr::BoolLit { value, span } => L1Expr::BoolLit { value, span },
        Expr::Var { name, span } => L1Expr::Var { name, span },
        Expr::UnaryOp { op, arg, span } => L1Expr::UnaryOp {
            op,
            arg: Box::new(l1_desug(*arg)),
            span,
        },
        // Desugaring Rule: Linear-Seq to Binary-Seq 
        // seq(a; b; c) -> seq_op(a, seq_op(b, c))
        // After this, there will be no more Linear-Seq's in the AST
        Expr::Seq{seq, span} => {
            fn seq_to_binary_op(mut seq: Vec<Expr>, span: (usize, usize)) -> Expr {
                if seq.len() == 0 {
                    Expr::UnitLit{span}
                }
                else if seq.len() == 1 {
                    seq.remove(0)
                }
                else {
                    let first = seq.remove(0);
                    let rest: Vec<Expr> = seq;
                    let rest_span = (first.span().1, rest.last().unwrap().span().1);
                    Expr::BinaryOp {
                        op: BinaryOp::Seq,
                        arg1: Box::new(first),
                        arg2: Box::new(seq_to_binary_op(rest, rest_span)),
                        span: span,
                    }
                }
                // match seq {
                //     [] => Expr::UnitLit{span: (0, 0)},
                //     [e] => seq[0],
                //     [first, rest] => {
                //         let span = if rest. == 0 {
                //             (first.0, first.1)
                //         } else {
                //             (first.0, rest.last().unwrap().1)
                //         };
                //         Expr::BinaryOp {
                //             op: BinaryOp::Seq,
                //             arg1: first,
                //             arg2: Box::new(seq_to_binary_op(rest)),
                //             span: span,
                //         }
                //     }
                // }
            }
            l1_desug(seq_to_binary_op(seq, span))
        }
        Expr::BinaryOp {
            op,
            arg1,
            arg2,
            span,
        } => {
            // Desugaring Rule: Let-Lifting
            //
            //    ;
            //  /   \
            // let  X
            //
            // becomes
            //
            //   let
            //    |
            //    X
            //
            if let (
                BinaryOp::Seq,
                Expr::Let {
                    name,
                    ty,
                    expr,
                    span,
                    ..
                },
            ) = (op.clone(), *arg1.clone())
            {
                L1Expr::Let {
                    name: name.clone(),
                    ty: ty.clone(),
                    expr: Box::new(l1_desug(*expr)),
                    body: Box::new(l1_desug(*arg2)),
                    span: span.clone(),
                }
            }
            // the trivial case
            else {
                L1Expr::BinaryOp {
                    op,
                    arg1: Box::new(l1_desug(*arg1)),
                    arg2: Box::new(l1_desug(*arg2)),
                    span,
                }
            }
        }
        Expr::App { func, args, span } => L1Expr::App {
            func: Box::new(l1_desug(*func)),
            args: {
                let mut result = Vec::new();
                for arg in args {
                    let desugd = l1_desug(arg);
                    result.push(desugd);
                }
                result
            },
            span,
        },
        Expr::Func {
            params,
            ret_ty,
            body,
            span,
        } => L1Expr::Func {
            params,
            ret_ty,
            body: Box::new(l1_desug(*body)),
            span,
        },
        Expr::Let {
            name,
            ty,
            expr,
            body,
            span,
        } => panic!("Let Expr can never be here. All Lets' should have been desugared by the let-lifting rule.
        If you see this, that probably means this Let did not show up as a left child of a Binary-Seq.
        A parser bug maybe?
        "),
    }
}

pub fn parse(input: &str) {
    match syntax::ExprParser::new().parse(input) {
        Ok(expr) => {
            println!("Parsed successfully: {:?}", expr);
            let expr1 = l1_desug(expr);
            println!("L1 Desug: {:?}", expr1);
        }
        Err(err) => {
            println!("Error parsing: {:?}", err);
        }
    }
}

#[test]
fn calculator1() {
    parse("let a = 1; 1; a + 2; 3");
    // syntax::ExprParser::new().parse("fn main(arg: bool) { 1 } (2, 3); { 1 }");
    // print!("{:?}",a.unwrap());
}
