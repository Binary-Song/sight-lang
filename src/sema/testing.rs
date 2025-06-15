use crate::ast::typed::{self, Typed};
use crate::ast::visitor::Visitor;
use crate::parser::context::{ConstraintHandle, Context};
use crate::parser::Parser;
use crate::sema::typing::{unify_constraints, TypingErr};
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
    let mut parser = Parser::new(src);
    let expr = parser.expr().unwrap();
    let ctx = Context::new_with_builtins();
    let mut expr = expr
        .to_typed_with_unsolved_constraints(ctx.clone())
        .unwrap();
    let state = ctx.state();
    let cons = &mut state.borrow_mut().constraints;
    let subs = unify_constraints(cons).unwrap();
    subs.apply_to_ast(&mut expr);
    println!(
        "typed expr literal value: \n\n{expr}\n\n",
        expr = expr.literal_value()
    );
    assert_eq!(expr, typed_expr);
}

fn expect_typing_error(src: &'static str, check_err: impl Fn(TypingErr) -> bool) {
    let mut parser = Parser::new(src);
    let expr = parser.expr().unwrap();
    let ctx = Context::new_with_builtins();
    let mut err = expr
        .to_typed_with_unsolved_constraints(ctx.clone())
        .unwrap_err();
    assert!(check_err(err));
}

mod cases {
    use super::check_type_inference_result;
    use super::check_type_inference_result_dummy;
    use crate::ast::typed;
    use crate::ast::typed::*;
    use crate::parser::context::Bindable;
    use crate::parser::context::Binding;
    use crate::parser::context::ConstraintHandle;
    use crate::sema::testing::expect_typing_error;
    use crate::sema::typing::TypingErr;
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
    fn type_expr_recur_fn() {
        let ex = Expr::Seq {
            seq: vec![
                Expr::Func {
                    func: Box::new(Func {
                        name: "a".to_string(),
                        param: Pattern::Tuple {
                            elems: vec![],
                            ty: Type::Tuple { elems: vec![] },
                            span: (19, 21),
                        },
                        ret_ty: Type::Int,
                        func_ty: Type::Arrow {
                            lhs: Box::new(Type::Tuple { elems: vec![] }),
                            rhs: Box::new(Type::Int),
                        },
                        body: Expr::Seq {
                            seq: vec![Expr::Application {
                                callee: Box::new(Expr::Var {
                                    name: "b".to_string(),
                                    span: (31, 32),
                                    ty: Type::Arrow {
                                        lhs: Box::new(Type::Tuple { elems: vec![] }),
                                        rhs: Box::new(Type::Int),
                                    },
                                }),
                                arg: Box::new(Expr::Tuple {
                                    elems: vec![],
                                    ty: Type::Tuple { elems: vec![] },
                                    span: (32, 34),
                                }),
                                ty: Type::Int,
                                cons: ConstraintHandle::new(0),
                                span: (31, 34),
                            }],
                            ty: Type::Int,
                            span: (36, 36),
                        },
                        span: (15, 36),
                    }),
                },
                Expr::Func {
                    func: Box::new(Func {
                        name: "b".to_string(),
                        param: Pattern::Tuple {
                            elems: vec![],
                            ty: Type::Tuple { elems: vec![] },
                            span: (42, 44),
                        },
                        ret_ty: Type::Int,
                        func_ty: Type::Arrow {
                            lhs: Box::new(Type::Tuple { elems: vec![] }),
                            rhs: Box::new(Type::Int),
                        },
                        body: Expr::Seq {
                            seq: vec![Expr::Application {
                                callee: Box::new(Expr::Var {
                                    name: "a".to_string(),
                                    span: (54, 55),
                                    ty: Type::Arrow {
                                        lhs: Box::new(Type::Tuple { elems: vec![] }),
                                        rhs: Box::new(Type::Int),
                                    },
                                }),
                                arg: Box::new(Expr::Tuple {
                                    elems: vec![],
                                    ty: Type::Tuple { elems: vec![] },
                                    span: (55, 57),
                                }),
                                ty: Type::Int,
                                cons: ConstraintHandle::new(1),
                                span: (54, 57),
                            }],
                            ty: Type::Int,
                            span: (59, 59),
                        },
                        span: (38, 59),
                    }),
                },
                Expr::Let {
                    lhs: Pattern::Var {
                        name: "v".to_string(),
                        ty: Type::Int,
                        span: (6, 7),
                    },
                    rhs: Box::new(Expr::Application {
                        callee: Box::new(Expr::Var {
                            name: "b".to_string(),
                            span: (10, 11),
                            ty: Type::Arrow {
                                lhs: Box::new(Type::Tuple { elems: vec![] }),
                                rhs: Box::new(Type::Int),
                            },
                        }),
                        arg: Box::new(Expr::Tuple {
                            elems: vec![],
                            ty: Type::Tuple { elems: vec![] },
                            span: (11, 13),
                        }),
                        ty: Type::Int,
                        cons: ConstraintHandle::new(2),
                        span: (10, 13),
                    }),
                    body: Box::new(Expr::Seq {
                        seq: vec![Expr::Tuple {
                            elems: vec![],
                            ty: Type::Tuple { elems: vec![] },
                            span: (60, 61),
                        }],
                        ty: Type::Tuple { elems: vec![] },
                        span: (61, 61),
                    }),
                    span: (2, 61),
                    cons: ConstraintHandle::new(3),
                },
            ],
            ty: Type::Tuple { elems: vec![] },
            span: (13, 61),
        };
        check_type_inference_result(
            "{ let v = b(); fn a() -> int { b() }; fn b() -> int { a() } }",
            ex,
        );
    }

    #[test]
    fn type_expr_dup_names() {
        expect_typing_error("{ fn a() -> () {} fn a() -> () {} }", |err| match err {
            TypingErr::DuplicateBinding { .. } => true,
            _ => false,
        });
    }
}
