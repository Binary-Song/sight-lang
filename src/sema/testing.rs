use crate::ast::typed::{self, Typed};
use crate::ast::visitor::Visitor;
use crate::parser::context::{ConstraintHandle, Context};
use crate::parser::Parser;
use crate::sema::typing::unify_constraints;
use crate::LiteralValue;

struct _TypePrinter {}

impl Visitor<()> for _TypePrinter {
    fn visit_texpr(&self, expr: &mut crate::ast::typed::Expr) -> Result<(), ()> {
        println!("type of {expr} is {ty}", expr = expr, ty = expr.ty());
        Ok(())
    }
}
fn check_type_inference_result_dummy(src: &'static str) {
    check_type_inference_result(
        src,
        typed::Expr::Lit {
            value: typed::Lit::Bool(true),
            span: (0, 0),
        },
    );
}

fn check_type_inference_result(src: &'static str, typed_expr: typed::Expr) {
    // "{ let b = (1, 1); let (c, d) = b;  }"
    let mut parser = Parser::new(src);
    let expr = parser.expr().unwrap();
    let ctx = Context::new_with_builtins();
    let mut expr = expr
        .to_typed_with_unsolved_constraints(ctx.clone())
        .unwrap();
    let mut constraints: std::collections::VecDeque<_> =
        ctx.constraints().borrow().clone().into_iter().collect();
    let subs = unify_constraints(&mut constraints).unwrap();
    subs.apply_to_ast(&mut expr);
    println!("typed expr = {expr}", expr = expr.literal_value());
    assert_eq!(expr, typed_expr);
}

mod cases {

    use super::check_type_inference_result;
    use crate::ast::typed::*;
    use crate::ast::typed::{self, Typed};
    use crate::ast::visitor::Visitor;
    use crate::parser::context::{ConstraintHandle, Context};
    use crate::parser::Parser;
    use crate::sema::testing::check_type_inference_result_dummy;
    use crate::sema::typing::unify_constraints;
    use crate::LiteralValue;

    #[test]
    fn type_expr() {
        check_type_inference_result(
            "{ let b = (1, 1); let (c, d) = b;  }",
            Expr::Seq {
                seq: vec![Expr::Let {
                    lhs: Pattern::Var {
                        name: "b".to_string(),
                        ty: Type::Tuple {
                            elems: vec![Type::Int, Type::Int],
                        },
                        span: (6, 7),
                    },
                    rhs: Box::new(Expr::Tuple {
                        elems: vec![
                            Expr::Lit {
                                value: Lit::Int(1),
                                span: (11, 12),
                            },
                            Expr::Lit {
                                value: Lit::Int(1),
                                span: (14, 15),
                            },
                        ],
                        ty: Type::Tuple {
                            elems: vec![Type::Int, Type::Int],
                        },
                        span: (11, 15),
                    }),
                    body: Box::new(Expr::Seq {
                        seq: vec![Expr::Let {
                            lhs: Pattern::Tuple {
                                elems: vec![
                                    Pattern::Var {
                                        name: "c".to_string(),
                                        ty: Type::Int,
                                        span: (23, 24),
                                    },
                                    Pattern::Var {
                                        name: "d".to_string(),
                                        ty: Type::Int,
                                        span: (26, 27),
                                    },
                                ],
                                ty: Type::Tuple {
                                    elems: vec![Type::Int, Type::Int],
                                },
                                span: (23, 27),
                            },
                            rhs: Box::new(Expr::Var {
                                name: "b".to_string(),
                                span: (31, 32),
                                ty: Type::Tuple {
                                    elems: vec![Type::Int, Type::Int],
                                },
                            }),
                            body: Box::new(Expr::Seq {
                                seq: vec![Expr::unit((35, 36))],
                                ty: Type::Tuple { elems: vec![] },
                                span: (36, 36),
                            }),
                            span: (18, 36),
                            cons: ConstraintHandle::new(0),
                        }],
                        ty: Type::unit(),
                        span: (32, 36),
                    }),
                    span: (2, 36),
                    cons: ConstraintHandle::new(1),
                }],
                ty: Type::unit(),
                span: (15, 36),
            },
        );
    }

    #[test]
    fn type_expr2() {
        check_type_inference_result_dummy("{ let b = a(); fn a() -> int { 1 } }");
    }
}
