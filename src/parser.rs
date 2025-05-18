lalrpop_mod!(pub syntax);
use crate::ast::*;
use crate::utils::*;
pub use lalrpop_util::lalrpop_mod;
use std::collections::VecDeque;
use std::vec;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Context {
    pub bindings: Vec<Binding>,
}

fn l1_desug(e: Expr, mut ctx: Context) -> Term {
    match e {
        Expr::UnitLit { span } => Term::Literal { value: Literal::Unit, span },
        Expr::IntLit { value, span } => Term::Literal { value: Literal::Int(value), span },
        Expr::BoolLit { value, span } => Term::Literal { value: Literal::Bool(value), span },
        Expr::Var { name, span } =>  {
            Term::Var { name, span }
        }
        Expr::UnaryOp { op, arg, span } => Term::App {
            callee: Box::new(Term::Op { op: Op::UnaryOp(op), span: span }), args: vec![l1_desug(*arg, ctx)], span: span
        },
        Expr::BinaryOp {
            op,
            arg1,
            arg2,
            span,
        } => Term::App {
            callee:Box::new(Term::Op { op: Op::BinaryOp(op), span: span }), args: vec![l1_desug(*arg1, ctx.clone()), l1_desug(*arg2, ctx)], span: span
        },
        Expr::Seq{seq, span: span_seq} => {
            let mut res_seq: Vec<Term> = vec![];
            // copy the seq to a deque
            let mut seq = VecDeque::from(seq);
            while let Some(expr) = seq.pop_front() {
                if let Expr::Let { name, ty, init, span: span_let } = expr {
                    // init-expr CANNOT use the 'let' variable
                    let init = l1_desug(*init, ctx.clone());
                    // body CAN use the 'let' variable
                    ctx.bindings.push(Binding{name: name.clone(), ty: ty.clone(), span: span_let});
                    // body is what is left of the sequence
                    let body = l1_desug(Expr::Seq { seq: Vec::from(seq), span: (span_let.1, span_seq.1) }, ctx);
                    res_seq.push(Term::Let { name: name, ty: ty, rhs: Box::new(init), body: Box::new(body), span: span_let.clone() });
                    // no need to continue, we already desugared the rest of the sequence as the body
                    return Term::Seq { seq: Vec::from(res_seq) , span: span_seq} ;
                } else {
                    res_seq.push(l1_desug(expr, ctx.clone()));
                }
            };
            return Term::Seq { seq: Vec::from(res_seq) , span: span_seq} ;
        },
        Expr::App { func, args, span } => Term::App {
            callee:  (Box::new(l1_desug(*func, ctx.clone()))),
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
            let mut ctx = ctx.clone();
            // add param bindings to context
            for p in params.iter() {
                ctx.bindings.push(Binding {
                        name: p.name.clone(),
                        ty: p.ty.clone(),
                        span: p.span,
                });
            }
            Term::Func {
            params,
            ret_ty,
            body: Box::new(l1_desug(*body, ctx)),
            span,
        }},
        Expr::Let {..} => panic!("Let Expr can never be here. All Lets' should have been desugared by the let-lifting rule.
        If you see this, that probably means this Let did not show up as a left child of a Binary-Seq.
        A parser bug maybe?
        "),
    }
}

type ParserError<'a> =
    lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'a>, &'static str>;

pub fn parse(input: &str) -> Result<Term, ParserError> {
    let e =  syntax::ExprParser::new().parse(input)?;
    let r = l1_desug(e, Context { bindings: vec![] });
    Ok(r)
}

#[test]
fn test1() {
    let r0: Term = parse(
        "{
       let a = { 1 + 1; let b = a + 2; };
       3
    }",
    )
    .unwrap();
    print!("L1: \n{}\n", r0.clone().print_v1());
}
