lalrpop_mod!(pub syntax);
use crate::ast::*;
use crate::ast_utils::*;
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
            panic!("Variable {} at {:?} is unbound. (todo: make this a proper error message)", name, span);
        }
        Expr::UnaryOp { op, arg, span } => L1Expr::UnaryOp {
            op,
            arg: Box::new(l1_desug(*arg, ctx)),
            span,
        },
        Expr::Seq{seq, span: span_seq} => {
            let result_exprs = vec![];
            let seq = VecDeque::from(seq);
            while let Some(expr) = seq.pop_front() {
                if let Expr::Let { name, ty, init, body, span: span_let } = expr {
                    // init-expr CANNOT use the 'let' variable
                    let init = l1_desug(*init, ctx);
                    // body CAN use the 'let' variable
                    ctx.push_front(Binding{name: name.clone(), ty: ty.clone(), span: span_let.clone()});
                    // body is what is left of the sequence
                    let body = l1_desug(Expr::Seq { seq: Vec::from(seq), span: (span_let.1, span_seq.1) }, ctx);
                    result_exprs.push(L1Expr::Let { name: name, ty: ty, init: Box::new(init), body: Box::new(body), span: span_let.clone() });
                    // no need to continue, we already desugared the rest of the sequence as the body
                    return L1Expr::Seq { seq: Vec::from(result_exprs) , span: span_seq} ;
                } else {
                    let e = l1_desug(expr, ctx.clone());
                    result_exprs.push(e);
                }
            };
            return L1Expr::Seq { seq: Vec::from(result_exprs) , span: span_seq} ;
        }
        Expr::BinaryOp {
            op,
            arg1,
            arg2,
            span,
        } => {
            
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
}
