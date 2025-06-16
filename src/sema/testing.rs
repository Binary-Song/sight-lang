use crate::ast::typed::{self, Typed};
use crate::ast::visitor::Visitor;
use crate::parser::context::Path;
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
    use crate::parser::context::Binding;
    use crate::parser::context::ConstraintHandle;
    use crate::parser::context::Path;
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
                                path: Path::LocalVar { index: 0 },
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
    fn recur_fn() {
        let ex = Expr::Seq {
            seq: vec![
                Expr::Func {
                    func: Box::new(Func {
                        name: QualifiedName {
                            names: vec![Name::Index(0), Name::String("a".to_string())],
                        },
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
                            seq: vec![Expr::App {
                                callee: Box::new(Expr::Var {
                                    name: "b".to_string(),
                                    span: (31, 32),
                                    ty: Type::Arrow {
                                        lhs: Box::new(Type::Tuple { elems: vec![] }),
                                        rhs: Box::new(Type::Int),
                                    },
                                    path: Path::Func {
                                        qual_name: QualifiedName {
                                            names: vec![
                                                Name::Index(0),
                                                Name::String("b".to_string()),
                                            ],
                                        },
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
                        name: QualifiedName {
                            names: vec![Name::Index(0), Name::String("b".to_string())],
                        },
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
                            seq: vec![Expr::App {
                                callee: Box::new(Expr::Var {
                                    name: "a".to_string(),
                                    span: (54, 55),
                                    ty: Type::Arrow {
                                        lhs: Box::new(Type::Tuple { elems: vec![] }),
                                        rhs: Box::new(Type::Int),
                                    },
                                    path: Path::Func {
                                        qual_name: QualifiedName {
                                            names: vec![
                                                Name::Index(0),
                                                Name::String("a".to_string()),
                                            ],
                                        },
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
                    rhs: Box::new(Expr::App {
                        callee: Box::new(Expr::Var {
                            name: "b".to_string(),
                            span: (10, 11),
                            ty: Type::Arrow {
                                lhs: Box::new(Type::Tuple { elems: vec![] }),
                                rhs: Box::new(Type::Int),
                            },
                            path: Path::Func {
                                qual_name: QualifiedName {
                                    names: vec![Name::Index(0), Name::String("b".to_string())],
                                },
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
    fn dup_names() {
        expect_typing_error("{ fn a() -> () {} fn a() -> () {} }", |err| match err {
            TypingErr::DuplicateBinding { .. } => true,
            _ => false,
        });
    }

    #[test]
    fn nested_fn1() {
        check_type_inference_result(
            "{
                fn a() -> () {
                    fn b() -> () {
                        fn a() -> () {}
                        a();
                        b();
                    }
                    fn d() -> () { d() } 
                }
            }",
            Expr::Seq {
                seq: vec![
                    Expr::Func {
                        func: Box::new(Func {
                            name: QualifiedName {
                                names: vec![Name::Index(0), Name::String("a".to_string())],
                            },
                            param: Pattern::Tuple {
                                elems: vec![],
                                ty: Type::Tuple { elems: vec![] },
                                span: (22, 24),
                            },
                            ret_ty: Type::Tuple { elems: vec![] },
                            func_ty: Type::Arrow {
                                lhs: Box::new(Type::Tuple { elems: vec![] }),
                                rhs: Box::new(Type::Tuple { elems: vec![] }),
                            },
                            body: Expr::Seq {
                                seq: vec![
                                    Expr::Func {
                                        func: Box::new(Func {
                                            name: QualifiedName {
                                                names: vec![
                                                    Name::Index(0),
                                                    Name::String("a".to_string()),
                                                    Name::String("b".to_string()),
                                                ],
                                            },
                                            param: Pattern::Tuple {
                                                elems: vec![],
                                                ty: Type::Tuple { elems: vec![] },
                                                span: (57, 59),
                                            },
                                            ret_ty: Type::Tuple { elems: vec![] },
                                            func_ty: Type::Arrow {
                                                lhs: Box::new(Type::Tuple { elems: vec![] }),
                                                rhs: Box::new(Type::Tuple { elems: vec![] }),
                                            },
                                            body: Expr::Seq {
                                                seq: vec![
                                                    Expr::Func {
                                                        func: Box::new(Func {
                                                            name: QualifiedName {
                                                                names: vec![
                                                                    Name::Index(0),
                                                                    Name::String("a".to_string()),
                                                                    Name::String("b".to_string()),
                                                                    Name::String("a".to_string()),
                                                                ],
                                                            },
                                                            param: Pattern::Tuple {
                                                                elems: vec![],
                                                                ty: Type::Tuple { elems: vec![] },
                                                                span: (96, 98),
                                                            },
                                                            ret_ty: Type::Tuple { elems: vec![] },
                                                            func_ty: Type::Arrow {
                                                                lhs: Box::new(Type::Tuple {
                                                                    elems: vec![],
                                                                }),
                                                                rhs: Box::new(Type::Tuple {
                                                                    elems: vec![],
                                                                }),
                                                            },
                                                            body: Expr::Seq {
                                                                seq: vec![Expr::Tuple {
                                                                    elems: vec![],
                                                                    ty: Type::Tuple {
                                                                        elems: vec![],
                                                                    },
                                                                    span: (106, 107),
                                                                }],
                                                                ty: Type::Tuple { elems: vec![] },
                                                                span: (107, 107),
                                                            },
                                                            span: (92, 107),
                                                        }),
                                                    },
                                                    Expr::App {
                                                        callee: Box::new(Expr::Var {
                                                            name: "a".to_string(),
                                                            span: (132, 133),
                                                            ty: Type::Arrow {
                                                                lhs: Box::new(Type::Tuple {
                                                                    elems: vec![],
                                                                }),
                                                                rhs: Box::new(Type::Tuple {
                                                                    elems: vec![],
                                                                }),
                                                            },
                                                            path: Path::Func {
                                                                qual_name: QualifiedName {
                                                                    names: vec![
                                                                        Name::Index(0),
                                                                        Name::String(
                                                                            "a".to_string(),
                                                                        ),
                                                                        Name::String(
                                                                            "b".to_string(),
                                                                        ),
                                                                        Name::String(
                                                                            "a".to_string(),
                                                                        ),
                                                                    ],
                                                                },
                                                            },
                                                        }),
                                                        arg: Box::new(Expr::Tuple {
                                                            elems: vec![],
                                                            ty: Type::Tuple { elems: vec![] },
                                                            span: (133, 135),
                                                        }),
                                                        ty: Type::Tuple { elems: vec![] },
                                                        cons: ConstraintHandle::new(0),
                                                        span: (132, 135),
                                                    },
                                                    Expr::App {
                                                        callee: Box::new(Expr::Var {
                                                            name: "b".to_string(),
                                                            span: (161, 162),
                                                            ty: Type::Arrow {
                                                                lhs: Box::new(Type::Tuple {
                                                                    elems: vec![],
                                                                }),
                                                                rhs: Box::new(Type::Tuple {
                                                                    elems: vec![],
                                                                }),
                                                            },
                                                            path: Path::Func {
                                                                qual_name: QualifiedName {
                                                                    names: vec![
                                                                        Name::Index(0),
                                                                        Name::String(
                                                                            "a".to_string(),
                                                                        ),
                                                                        Name::String(
                                                                            "b".to_string(),
                                                                        ),
                                                                    ],
                                                                },
                                                            },
                                                        }),
                                                        arg: Box::new(Expr::Tuple {
                                                            elems: vec![],
                                                            ty: Type::Tuple { elems: vec![] },
                                                            span: (162, 164),
                                                        }),
                                                        ty: Type::Tuple { elems: vec![] },
                                                        cons: ConstraintHandle::new(1),
                                                        span: (161, 164),
                                                    },
                                                    Expr::Tuple {
                                                        elems: vec![],
                                                        ty: Type::Tuple { elems: vec![] },
                                                        span: (186, 187),
                                                    },
                                                ],
                                                ty: Type::Tuple { elems: vec![] },
                                                span: (187, 187),
                                            },
                                            span: (53, 187),
                                        }),
                                    },
                                    Expr::Func {
                                        func: Box::new(Func {
                                            name: QualifiedName {
                                                names: vec![
                                                    Name::Index(0),
                                                    Name::String("a".to_string()),
                                                    Name::String("d".to_string()),
                                                ],
                                            },
                                            param: Pattern::Tuple {
                                                elems: vec![],
                                                ty: Type::Tuple { elems: vec![] },
                                                span: (212, 214),
                                            },
                                            ret_ty: Type::Tuple { elems: vec![] },
                                            func_ty: Type::Arrow {
                                                lhs: Box::new(Type::Tuple { elems: vec![] }),
                                                rhs: Box::new(Type::Tuple { elems: vec![] }),
                                            },
                                            body: Expr::Seq {
                                                seq: vec![Expr::App {
                                                    callee: Box::new(Expr::Var {
                                                        name: "d".to_string(),
                                                        span: (223, 224),
                                                        ty: Type::Arrow {
                                                            lhs: Box::new(Type::Tuple {
                                                                elems: vec![],
                                                            }),
                                                            rhs: Box::new(Type::Tuple {
                                                                elems: vec![],
                                                            }),
                                                        },
                                                        path: Path::Func {
                                                            qual_name: QualifiedName {
                                                                names: vec![
                                                                    Name::Index(0),
                                                                    Name::String("a".to_string()),
                                                                    Name::String("d".to_string()),
                                                                ],
                                                            },
                                                        },
                                                    }),
                                                    arg: Box::new(Expr::Tuple {
                                                        elems: vec![],
                                                        ty: Type::Tuple { elems: vec![] },
                                                        span: (224, 226),
                                                    }),
                                                    ty: Type::Tuple { elems: vec![] },
                                                    cons: ConstraintHandle::new(2),
                                                    span: (223, 226),
                                                }],
                                                ty: Type::Tuple { elems: vec![] },
                                                span: (228, 228),
                                            },
                                            span: (208, 228),
                                        }),
                                    },
                                    Expr::Tuple {
                                        elems: vec![],
                                        ty: Type::Tuple { elems: vec![] },
                                        span: (246, 247),
                                    },
                                ],
                                ty: Type::Tuple { elems: vec![] },
                                span: (247, 247),
                            },
                            span: (18, 247),
                        }),
                    },
                    Expr::Tuple {
                        elems: vec![],
                        ty: Type::Tuple { elems: vec![] },
                        span: (260, 261),
                    },
                ],
                ty: Type::Tuple { elems: vec![] },
                span: (261, 261),
            },
        );
    }

    #[test]
    fn nested_fn2() {
        check_type_inference_result(
            "{ { fn a () -> () {  } } { fn b () -> () {  } }  }",
            Expr::Seq {
                seq: vec![
                    Expr::Seq {
                        seq: vec![
                            Expr::Func {
                                func: Box::new(Func {
                                    name: QualifiedName {
                                        names: vec![
                                            Name::Index(0),
                                            Name::Index(0),
                                            Name::String("a".to_string()),
                                        ],
                                    },
                                    param: Pattern::Tuple {
                                        elems: vec![],
                                        ty: Type::Tuple { elems: vec![] },
                                        span: (9, 11),
                                    },
                                    ret_ty: Type::Tuple { elems: vec![] },
                                    func_ty: Type::Arrow {
                                        lhs: Box::new(Type::Tuple { elems: vec![] }),
                                        rhs: Box::new(Type::Tuple { elems: vec![] }),
                                    },
                                    body: Expr::Seq {
                                        seq: vec![Expr::Tuple {
                                            elems: vec![],
                                            ty: Type::Tuple { elems: vec![] },
                                            span: (21, 22),
                                        }],
                                        ty: Type::Tuple { elems: vec![] },
                                        span: (22, 22),
                                    },
                                    span: (4, 22),
                                }),
                            },
                            Expr::Tuple {
                                elems: vec![],
                                ty: Type::Tuple { elems: vec![] },
                                span: (23, 24),
                            },
                        ],
                        ty: Type::Tuple { elems: vec![] },
                        span: (24, 24),
                    },
                    Expr::Seq {
                        seq: vec![
                            Expr::Func {
                                func: Box::new(Func {
                                    name: QualifiedName {
                                        names: vec![
                                            Name::Index(0),
                                            Name::Index(1),
                                            Name::String("b".to_string()),
                                        ],
                                    },
                                    param: Pattern::Tuple {
                                        elems: vec![],
                                        ty: Type::Tuple { elems: vec![] },
                                        span: (32, 34),
                                    },
                                    ret_ty: Type::Tuple { elems: vec![] },
                                    func_ty: Type::Arrow {
                                        lhs: Box::new(Type::Tuple { elems: vec![] }),
                                        rhs: Box::new(Type::Tuple { elems: vec![] }),
                                    },
                                    body: Expr::Seq {
                                        seq: vec![Expr::Tuple {
                                            elems: vec![],
                                            ty: Type::Tuple { elems: vec![] },
                                            span: (44, 45),
                                        }],
                                        ty: Type::Tuple { elems: vec![] },
                                        span: (45, 45),
                                    },
                                    span: (27, 45),
                                }),
                            },
                            Expr::Tuple {
                                elems: vec![],
                                ty: Type::Tuple { elems: vec![] },
                                span: (46, 47),
                            },
                        ],
                        ty: Type::Tuple { elems: vec![] },
                        span: (47, 47),
                    },
                    Expr::Tuple {
                        elems: vec![],
                        ty: Type::Tuple { elems: vec![] },
                        span: (49, 50),
                    },
                ],
                ty: Type::Tuple { elems: vec![] },
                span: (50, 50),
            },
        )
    }

    #[test]
    fn nested_fn_cannot_access_outer_vars() {
        expect_typing_error(
            "{ fn a() -> () { let b = 1; fn c() -> int { b } } }",
            |err| match err {
                TypingErr::UnboundVar { .. } => true,
                _ => false,
            },
        );
    }
}
