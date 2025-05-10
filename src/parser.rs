lalrpop_mod!(pub syntax);
use crate::ast::*;
use crate::ast_tools::*;
pub use lalrpop_util::lalrpop_mod;
use std::collections::vec_deque;
use std::collections::VecDeque;
use std::fmt::format;
use std::vec;
use std::{f64::consts::E, fmt::Binary};



fn l1_desug(e: Expr, ctx: VecDeque<Binding>) -> L1Expr {
    match e {
        Expr::UnitLit { span } => L1Expr::UnitLit { span },
        Expr::IntLit { value, span } => L1Expr::IntLit { value, span },
        Expr::BoolLit { value, span } => L1Expr::BoolLit { value, span },
        Expr::Var { name, span } => 
        {
            for (index, bind) in ctx.iter().enumerate() {
                if bind.name == name {
                    return L1Expr::Var { index, span, context_depth: ctx.len() };
                }
            }
            panic!("Variable {} at {:?} is unbound.", name, span);
        }
        Expr::UnaryOp { op, arg, span } => L1Expr::UnaryOp {
            op,
            arg: Box::new(l1_desug(*arg, ctx)),
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
            }
            l1_desug(seq_to_binary_op(seq, span), ctx)
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
                    init: expr,
                    span,
                    ..
                },
            ) = (op.clone(), *arg1.clone())
            {
                let mut ctx2 = ctx.clone();
                ctx2.push_front( Binding{name: name.clone(), ty: ty.clone(), span});
                L1Expr::Let {
                    name: name,
                    ty: ty,
                    init: Box::new(l1_desug(*expr, ctx)),
                    body: Box::new(l1_desug(*arg2, ctx2)),
                    span: span.clone(),
                }
            }
            // the trivial case
            else {
                L1Expr::BinaryOp {
                    op,
                    arg1: Box::new(l1_desug(*arg1, ctx.clone())),
                    arg2: Box::new(l1_desug(*arg2, ctx)),
                    span,
                }
            }
        }
        Expr::App { func, args, span } => L1Expr::App {
            func: Box::new(l1_desug(*func, ctx.clone())),
            args: {
                let mut result = Vec::new();
                for arg in args {
                    let desugd = l1_desug(arg, ctx.clone());
                    result.push(desugd);
                }
                result
            },
            span,
        } ,
        Expr::Func {
            params,
            ret_ty,
            body,
            span,
        } => {
            let mut ctx2 = ctx.clone();
            // add param bindings to context
            for p in params.iter() {
                ctx2.push_front(Binding {
                        name: p.name.clone(),
                        ty: p.ty.clone(),
                        span: p.span,
                });
            }
            L1Expr::Func {
            params,
            ret_ty,
            body: Box::new(l1_desug(*body, ctx2)),
            span,
        }},
        Expr::Let {
            name,
            ty,
            init: expr,
            body,
            span,
        } => panic!("Let Expr can never be here. All Lets' should have been desugared by the let-lifting rule.
        If you see this, that probably means this Let did not show up as a left child of a Binary-Seq.
        A parser bug maybe?
        "),
    }
}

type ParserError<'a> =
    lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'a>, &'static str>;

pub fn parse(input: &str) -> Result<Expr, ParserError> {
    syntax::ExprParser::new().parse(input)
}

#[test]
fn test1() {
    let r0 = parse(
        "{
       let a = { 1;2;};
       let b = { 3+4; 5*a };
       {
         {
           let x = fn (a: bool) { 6 + a * b };
           a + x + b
         }
       }
       8
    }",
    )
    .unwrap();
    print!("{}\n", r0.clone().print_v1());
    let r1 = l1_desug(r0.clone(), vec_deque::VecDeque::new());
    print!("{}\n", r1.clone().print_v1());

    assert_eq!(
        r0.print_v1(),
        r#""#
    );
    assert_eq!(
        r1.print_v1(),
        r#""#
    );
}
