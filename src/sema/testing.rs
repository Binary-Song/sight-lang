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

fn test_type_inference_result(src: &'static str, typed_expr: typed::Expr) {
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

#[test]
fn test_type_expr() {
    use typed::*;
    test_type_inference_result(
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
                            seq: vec![Expr::Lit {
                                value: Lit::Unit,
                                span: (35, 36),
                            }],
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
fn test_type_expr2() {
    use typed::*;
    test_type_inference_result(
        "{ let b = a(); fn a() -> int { 1 } }",
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
                            seq: vec![Expr::Lit {
                                value: Lit::Unit,
                                span: (35, 36),
                            }],
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
