lalrpop_mod!(pub syntax);
use crate::ast::*;
use crate::ast_tools::*;
pub use lalrpop_util::lalrpop_mod;
use std::fmt::format;
use std::{f64::consts::E, fmt::Binary};

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
                    init: expr,
                    span,
                    ..
                },
            ) = (op.clone(), *arg1.clone())
            {
                L1Expr::Let {
                    name: name.clone(),
                    ty: ty.clone(),
                    init: Box::new(l1_desug(*expr)),
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
       let b = { 3+4; 5*b };
       {
         {
           let x = fn (a: bool) { 6 };
         }
       }
       7
    }",
    )
    .unwrap();
    assert_eq!(
        r0.print_v1(),
        r#"<Seq><Item1><Let><Name>a</Name><Ty><None/></Ty><Expr><Seq><Item1><IntLit value="1"/></Item1><Item2><IntLit value="2"/></Item2><Item3><UnitLit/></Item3></Seq></Expr></Let></Item1><Item2><Let><Name>b</Name><Ty><None/></Ty><Expr><Seq><Item1><BinaryOp op="Add"><Arg1><IntLit value="3"/></Arg1><Arg2><IntLit value="4"/></Arg2></BinaryOp></Item1><Item2><BinaryOp op="Mul"><Arg1><IntLit value="5"/></Arg1><Arg2><Var name="b"/></Arg2></BinaryOp></Item2></Seq></Expr></Let></Item2><Item3><Seq><Item1><Seq><Item1><Let><Name>x</Name><Ty><None/></Ty><Expr><Func><Params><Item1><Binding Name="a"><Ty><Bool/></Ty></Binding></Item1></Params><RetTy><None/></RetTy><Body><Seq><Item1><IntLit value="6"/></Item1></Seq></Body></Func></Expr></Let></Item1><Item2><UnitLit/></Item2></Seq></Item1><Item2><UnitLit/></Item2></Seq></Item3><Item4><IntLit value="7"/></Item4></Seq>"#
    );
    let r1 = l1_desug(r0);
    assert_eq!(
        r1.print_v1(),
        r#"<Let><Name>a</Name><Ty><None/></Ty><Expr><BinaryOp op="Seq"><Arg1><IntLit value="1"/></Arg1><Arg2><BinaryOp op="Seq"><Arg1><IntLit value="2"/></Arg1><Arg2><UnitLit/></Arg2></BinaryOp></Arg2></BinaryOp></Expr><Body><Let><Name>b</Name><Ty><None/></Ty><Expr><BinaryOp op="Seq"><Arg1><BinaryOp op="Add"><Arg1><IntLit value="3"/></Arg1><Arg2><IntLit value="4"/></Arg2></BinaryOp></Arg1><Arg2><BinaryOp op="Mul"><Arg1><IntLit value="5"/></Arg1><Arg2><Var name="b"/></Arg2></BinaryOp></Arg2></BinaryOp></Expr><Body><BinaryOp op="Seq"><Arg1><BinaryOp op="Seq"><Arg1><Let><Name>x</Name><Ty><None/></Ty><Expr><Func><Params><Item1><Binding Name="a"><Ty><Bool/></Ty></Binding></Item1></Params><RetTy><None/></RetTy><Body><IntLit value="6"/></Body></Func></Expr><Body><UnitLit/></Body></Let></Arg1><Arg2><UnitLit/></Arg2></BinaryOp></Arg1><Arg2><IntLit value="7"/></Arg2></BinaryOp></Body></Let></Body></Let>"#
    );
}
