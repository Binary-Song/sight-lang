use crate::LiteralValue;
use crate::{ast::Expr, parser::Parser};
use pretty_assertions::assert_eq;
use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::Once;
use tracing::field::Field;
use tracing_subscriber;
use tracing_subscriber::fmt::{self, format::Writer, FormatEvent, FormatFields};
use tracing_subscriber::registry::LookupSpan;
use tracing_subscriber::util::SubscriberInitExt;
struct OnlyCurrentSpan;

impl<S, N> FormatEvent<S, N> for OnlyCurrentSpan
where
    S: tracing::Subscriber + for<'a> LookupSpan<'a>,
    N: for<'a> FormatFields<'a> + 'static,
{
    fn format_event(
        &self,
        ctx: &fmt::FmtContext<'_, S, N>,
        mut writer: Writer<'_>,
        event: &tracing::Event<'_>,
    ) -> std::fmt::Result {
        let mut dict = HashMap::new();
        event.record(&mut |field: &Field, value: &dyn Debug| {
            dict.insert(field.name().to_string(), format!("{value:?}"));
        });

        // get the scope and fields
        let (scope, args) = if let Some(scope) = ctx.event_scope() {
            if let Some(span) = scope.from_root().last() {
                if let Some(fields) = span
                    .extensions()
                    .get::<tracing_subscriber::fmt::FormattedFields<N>>()
                {
                    (span.name().to_string(), fields.fields.as_str().to_string())
                } else {
                    (span.name().to_string(), "".to_string())
                }
            } else {
                ("".to_string(), "".to_string())
            }
        } else {
            ("".to_string(), "".to_string())
        };
        // Filter out ANSI escape color characters from args
        let re = regex::Regex::new(r"\x1b\[[0-9;]*m").unwrap();
        let args = re.replace_all(&args, "").to_string();

        // get the message type
        let msg = dict
            .get("message")
            .map(|s| s.to_string())
            .unwrap_or_default();
        let ret = dict
            .get("return")
            .map(|s| s.to_string())
            .unwrap_or_default();
        let depth = if let Some(scope) = ctx.event_scope() {
            scope.from_root().count()
        } else {
            0
        };
        let sp = "  ".repeat(depth);
        let spp = "  ".repeat(depth + 1);
        let sppp = "  ".repeat(depth + 2);
        if msg == "\"enter\"" {
            write!(
                writer,
                "{sp}<{scope}>\n{spp}<args>\n{sppp}{args}\n{spp}</args>\n"
            )?;
        } else if msg == "" {
            write!(
                writer,
                "{spp}<return>\n{sppp}{ret}\n{spp}</return>\n{sp}</{scope}>\n"
            )?;
        }
        Ok(())
    }
}

static INIT: Once = Once::new();
pub fn init_testing() {
    INIT.call_once(|| {
        tracing_subscriber::fmt()
            .with_max_level(tracing::Level::DEBUG)
            .with_test_writer()
            .with_span_events(
                tracing_subscriber::fmt::format::FmtSpan::ENTER
                    | tracing_subscriber::fmt::format::FmtSpan::CLOSE,
            )
            .event_format(OnlyCurrentSpan)
            .finish()
            .init();
    });
}

fn assert_parse_expr(input: &str, expr: Expr) {
    init_testing();
    let mut parser = Parser::new(input);
    let actual = match parser.expr() {
        Ok(actual) => actual,
        Err(err) => {
            panic!("Err: {:?}, Parser tried: {:?}", err, parser.trials);
        }
    };
    let actual_str = actual.literal_value();
    println!("\nGot:\n{actual_str}\n");
    assert_eq!(actual, expr);
}

mod cases {
    use super::assert_parse_expr;
    use crate::ast::typed::Name;
    use crate::ast::Expr;
    use crate::ast::*;

    #[test]
    fn test_var() {
        assert_parse_expr(
            "x",
            Expr::Var {
                name: "x".to_string(),
                span: (0, 1),
            },
        );
    }

    #[test]
    fn test_0_arg_func() {
        assert_parse_expr(
            "{ fn foo() -> (int, int) { (1,2) } }",
            Expr::Block(Box::new(Block {
                name: typed::Name::Index(0),
                stmts: vec![
                    Stmt::Func(Box::new(Func {
                        name: "foo".to_string(),
                        param: Pattern::Unit { span: (8, 10) },
                        ret_ty: TypeExpr::Tuple {
                            elems: vec![
                                TypeExpr::Int { span: (15, 18) },
                                TypeExpr::Int { span: (20, 23) },
                            ],
                            span: (15, 23),
                        },
                        body: Expr::Block(Box::new(Block {
                            name: typed::Name::String("foo".to_string()),
                            stmts: vec![Stmt::Expr {
                                expr: Expr::Tuple {
                                    elems: vec![
                                        Expr::Int {
                                            value: 1,
                                            span: (28, 29),
                                        },
                                        Expr::Int {
                                            value: 2,
                                            span: (30, 31),
                                        },
                                    ],
                                    span: (28, 31),
                                },
                                span: (25, 34),
                            }],
                            span: (25, 34),
                        })),
                        span: (2, 34),
                    })),
                    Stmt::Expr {
                        expr: Expr::Unit { span: (35, 36) },
                        span: (35, 36),
                    },
                ],
                span: (0, 36),
            })),
        );
    }

    #[test]
    fn test_1_arg_func() {
        assert_parse_expr(
            "{ fn foo (a: int) -> int { a + 1 } 2 }",
            Expr::Block(Box::new(Block {
                name: typed::Name::Index(0),
                stmts: vec![
                    Stmt::Func(Box::new(Func {
                        name: "foo".to_string(),
                        param: Pattern::Var {
                            name: "a".to_string(),
                            ty: Some(TypeExpr::Int { span: (13, 16) }),
                            span: (10, 11),
                        },
                        ret_ty: TypeExpr::Int { span: (21, 24) },
                        body: Expr::Block(Box::new(Block {
                            name: typed::Name::String("foo".to_string()),
                            stmts: vec![Stmt::Expr {
                                expr: Expr::BinaryOp {
                                    op: BinaryOp::Add,
                                    lhs: Box::new(Expr::Var {
                                        name: "a".to_string(),
                                        span: (27, 28),
                                    }),
                                    rhs: Box::new(Expr::Int {
                                        value: 1,
                                        span: (31, 32),
                                    }),
                                    span: (27, 32),
                                    op_span: (29, 30),
                                },
                                span: (25, 34),
                            }],
                            span: (25, 34),
                        })),
                        span: (2, 34),
                    })),
                    Stmt::Expr {
                        expr: Expr::Int {
                            value: 2,
                            span: (35, 36),
                        },
                        span: (0, 38),
                    },
                ],
                span: (0, 38),
            })),
        )
    }

    #[test]
    fn test_2_arg_func() {
        assert_parse_expr(
            "{ fn foo(a: int, b: int) -> int { a + b + 1 } }",
            Expr::Block(Box::new(Block {
                name: typed::Name::Index(0),
                stmts: vec![
                    Stmt::Func(Box::new(Func {
                        name: "foo".to_string(),
                        param: Pattern::Tuple {
                            elems: vec![
                                Pattern::Var {
                                    name: "a".to_string(),
                                    ty: Some(TypeExpr::Int { span: (12, 15) }),
                                    span: (9, 10),
                                },
                                Pattern::Var {
                                    name: "b".to_string(),
                                    ty: Some(TypeExpr::Int { span: (20, 23) }),
                                    span: (17, 18),
                                },
                            ],
                            span: (9, 18),
                        },
                        ret_ty: TypeExpr::Int { span: (28, 31) },
                        body: Expr::Block(Box::new(Block {
                            name: typed::Name::String("foo".to_string()),
                            stmts: vec![Stmt::Expr {
                                expr: Expr::BinaryOp {
                                    op: BinaryOp::Add,
                                    lhs: Box::new(Expr::BinaryOp {
                                        op: BinaryOp::Add,
                                        lhs: Box::new(Expr::Var {
                                            name: "a".to_string(),
                                            span: (34, 35),
                                        }),
                                        rhs: Box::new(Expr::Var {
                                            name: "b".to_string(),
                                            span: (38, 39),
                                        }),
                                        span: (34, 39),
                                        op_span: (36, 37),
                                    }),
                                    rhs: Box::new(Expr::Int {
                                        value: 1,
                                        span: (42, 43),
                                    }),
                                    span: (34, 43),
                                    op_span: (40, 41),
                                },
                                span: (32, 45),
                            }],
                            span: (32, 45),
                        })),
                        span: (2, 45),
                    })),
                    Stmt::Expr {
                        expr: Expr::Unit { span: (46, 47) },
                        span: (46, 47),
                    },
                ],
                span: (0, 47),
            })),
        );
    }

    #[test]
    fn test_app() {
        assert_parse_expr(
            "
            { fn foo(a: int) -> int { 1 } foo 1 }",
            Expr::Block(Box::new(Block {
                name: typed::Name::Index(0),
                stmts: vec![
                    Stmt::Func(Box::new(Func {
                        name: "foo".to_string(),
                        param: Pattern::Var {
                            name: "a".to_string(),
                            ty: Some(TypeExpr::Int { span: (25, 28) }),
                            span: (22, 23),
                        },
                        ret_ty: TypeExpr::Int { span: (33, 36) },
                        body: Expr::Block(Box::new(Block {
                            name: typed::Name::String("foo".to_string()),
                            stmts: vec![Stmt::Expr {
                                expr: Expr::Int {
                                    value: 1,
                                    span: (39, 40),
                                },
                                span: (37, 42),
                            }],
                            span: (37, 42),
                        })),
                        span: (15, 42),
                    })),
                    Stmt::Expr {
                        expr: Expr::App {
                            func: Box::new(Expr::Var {
                                name: "foo".to_string(),
                                span: (43, 46),
                            }),
                            arg: Box::new(Expr::Int {
                                value: 1,
                                span: (47, 48),
                            }),
                            span: (43, 48),
                        },
                        span: (13, 50),
                    },
                ],
                span: (13, 50),
            })),
        );
    }

    #[test]
    fn test_mutual_recusive() {
        assert_parse_expr(
            "
            {
             fn foo() -> int { bar() }
             fn bar() -> int { foo() }
             foo() + bar()
            }",
            Expr::Block(Box::new(Block {
                name: typed::Name::Index(0),
                stmts: vec![
                    Stmt::Func(Box::new(Func {
                        name: "foo".to_string(),
                        param: Pattern::Unit { span: (34, 36) },
                        ret_ty: TypeExpr::Int { span: (40, 43) },
                        body: Expr::Block(Box::new(Block {
                            name: typed::Name::String("foo".to_string()),
                            stmts: vec![Stmt::Expr {
                                expr: Expr::App {
                                    func: Box::new(Expr::Var {
                                        name: "bar".to_string(),
                                        span: (46, 49),
                                    }),
                                    arg: Box::new(Expr::Unit { span: (49, 51) }),
                                    span: (46, 51),
                                },
                                span: (44, 53),
                            }],
                            span: (44, 53),
                        })),
                        span: (28, 53),
                    })),
                    Stmt::Func(Box::new(Func {
                        name: "bar".to_string(),
                        param: Pattern::Unit { span: (73, 75) },
                        ret_ty: TypeExpr::Int { span: (79, 82) },
                        body: Expr::Block(Box::new(Block {
                            name: typed::Name::String("bar".to_string()),
                            stmts: vec![Stmt::Expr {
                                expr: Expr::App {
                                    func: Box::new(Expr::Var {
                                        name: "foo".to_string(),
                                        span: (85, 88),
                                    }),
                                    arg: Box::new(Expr::Unit { span: (88, 90) }),
                                    span: (85, 90),
                                },
                                span: (83, 92),
                            }],
                            span: (83, 92),
                        })),
                        span: (67, 92),
                    })),
                    Stmt::Expr {
                        expr: Expr::BinaryOp {
                            op: BinaryOp::Add,
                            lhs: Box::new(Expr::App {
                                func: Box::new(Expr::Var {
                                    name: "foo".to_string(),
                                    span: (106, 109),
                                }),
                                arg: Box::new(Expr::Unit { span: (109, 111) }),
                                span: (106, 111),
                            }),
                            rhs: Box::new(Expr::App {
                                func: Box::new(Expr::Var {
                                    name: "bar".to_string(),
                                    span: (114, 117),
                                }),
                                arg: Box::new(Expr::Unit { span: (117, 119) }),
                                span: (114, 119),
                            }),
                            span: (106, 119),
                            op_span: (112, 113),
                        },
                        span: (13, 133),
                    },
                ],
                span: (13, 133),
            })),
        );
    }

    #[test]
    fn test_let() {
        assert_parse_expr(
            "{ let a : int = 1; let b : int = a + 1; b }",
            Expr::Block(Box::new(Block {
                name: typed::Name::Index(0),
                stmts: vec![
                    Stmt::Let {
                        lhs: Pattern::Var {
                            name: "a".to_string(),
                            ty: Some(TypeExpr::Int { span: (10, 13) }),
                            span: (6, 7),
                        },
                        rhs: Expr::Int {
                            value: 1,
                            span: (16, 17),
                        },
                        span: (2, 17),
                    },
                    Stmt::Let {
                        lhs: Pattern::Var {
                            name: "b".to_string(),
                            ty: Some(TypeExpr::Int { span: (27, 30) }),
                            span: (23, 24),
                        },
                        rhs: Expr::BinaryOp {
                            op: BinaryOp::Add,
                            lhs: Box::new(Expr::Var {
                                name: "a".to_string(),
                                span: (33, 34),
                            }),
                            rhs: Box::new(Expr::Int {
                                value: 1,
                                span: (37, 38),
                            }),
                            span: (33, 38),
                            op_span: (35, 36),
                        },
                        span: (19, 38),
                    },
                    Stmt::Expr {
                        expr: Expr::Var {
                            name: "b".to_string(),
                            span: (40, 41),
                        },
                        span: (0, 43),
                    },
                ],
                span: (0, 43),
            })),
        );
    }
    #[test]
    fn test_pattern_1() {
        assert_parse_expr(
            "{let (a: int, b: int) = (1,2);}",
            Expr::Block(Box::new(Block {
                name: typed::Name::Index(0),
                stmts: vec![
                    Stmt::Let {
                        lhs: Pattern::Tuple {
                            elems: vec![
                                Pattern::Var {
                                    name: "a".to_string(),
                                    ty: Some(TypeExpr::Int { span: (9, 12) }),
                                    span: (6, 7),
                                },
                                Pattern::Var {
                                    name: "b".to_string(),
                                    ty: Some(TypeExpr::Int { span: (17, 20) }),
                                    span: (14, 15),
                                },
                            ],
                            span: (6, 15),
                        },
                        rhs: Expr::Tuple {
                            elems: vec![
                                Expr::Int {
                                    value: 1,
                                    span: (25, 26),
                                },
                                Expr::Int {
                                    value: 2,
                                    span: (27, 28),
                                },
                            ],
                            span: (25, 28),
                        },
                        span: (1, 28),
                    },
                    Stmt::Expr {
                        expr: Expr::Unit { span: (30, 31) },
                        span: (30, 31),
                    },
                ],
                span: (0, 31),
            })),
        );
    }

    #[test]
    fn test_pattern_2() {
        assert_parse_expr(
            "{let (a: int, ()) = 1, ();}",
            Expr::Block(Box::new(Block {
                name: typed::Name::Index(0),
                stmts: vec![
                    Stmt::Let {
                        lhs: Pattern::Tuple {
                            elems: vec![
                                Pattern::Var {
                                    name: "a".to_string(),
                                    ty: Some(TypeExpr::Int { span: (9, 12) }),
                                    span: (6, 7),
                                },
                                Pattern::Unit { span: (14, 16) },
                            ],
                            span: (6, 16),
                        },
                        rhs: Expr::Tuple {
                            elems: vec![
                                Expr::Int {
                                    value: 1,
                                    span: (20, 21),
                                },
                                Expr::Unit { span: (23, 25) },
                            ],
                            span: (20, 25),
                        },
                        span: (1, 25),
                    },
                    Stmt::Expr {
                        expr: Expr::Unit { span: (26, 27) },
                        span: (26, 27),
                    },
                ],
                span: (0, 27),
            })),
        );
    }

    #[test]
    fn test_pattern_3() {
        assert_parse_expr(
            "{let c: int = a + b;}",
            Expr::Block(Box::new(Block {
                stmts: vec![
                    Stmt::Let {
                        lhs: Pattern::Var {
                            name: "c".to_string(),
                            ty: Some(TypeExpr::Int { span: (8, 11) }),
                            span: (5, 6),
                        },
                        rhs: Expr::BinaryOp {
                            op: BinaryOp::Add,
                            lhs: Box::new(Expr::Var {
                                name: "a".to_string(),
                                span: (14, 15),
                            }),
                            rhs: Box::new(Expr::Var {
                                name: "b".to_string(),
                                span: (18, 19),
                            }),
                            span: (14, 19),
                            op_span: (16, 17),
                        },
                        span: (1, 19),
                    },
                    Stmt::Expr {
                        expr: Expr::Unit { span: (20, 21) },
                        span: (20, 21),
                    },
                ],
                name: Name::Index(0),
                span: (0, 21),
            })),
        );
    }

    #[test]
    fn test_no_ty_anno() {
        assert_parse_expr(
            "{let (x, y) = (1, 2);}",
            Expr::Block(Box::new(Block {
                name: typed::Name::Index(0),
                stmts: vec![
                    Stmt::Let {
                        lhs: Pattern::Tuple {
                            elems: vec![
                                Pattern::Var {
                                    name: "x".to_string(),
                                    ty: None,
                                    span: (6, 7),
                                },
                                Pattern::Var {
                                    name: "y".to_string(),
                                    ty: None,
                                    span: (9, 10),
                                },
                            ],
                            span: (6, 10),
                        },
                        rhs: Expr::Tuple {
                            elems: vec![
                                Expr::Int {
                                    value: 1,
                                    span: (15, 16),
                                },
                                Expr::Int {
                                    value: 2,
                                    span: (18, 19),
                                },
                            ],
                            span: (15, 19),
                        },
                        span: (1, 19),
                    },
                    Stmt::Expr {
                        expr: Expr::Unit { span: (21, 22) },
                        span: (21, 22),
                    },
                ],
                span: (0, 22),
            })),
        );
    }

    #[test]
    fn test_nested_blocks() {
        assert_parse_expr(
            "{ { } { {} } fn a () -> () {} }",
            Expr::Block(Box::new(Block {
                stmts: vec![
                    Stmt::Block(Block {
                        stmts: vec![Stmt::Expr {
                            expr: Expr::Unit { span: (4, 5) },
                            span: (4, 5),
                        }],
                        name: Name::Index(0),
                        span: (2, 5),
                    }),
                    Stmt::Block(Block {
                        stmts: vec![
                            Stmt::Block(Block {
                                stmts: vec![Stmt::Expr {
                                    expr: Expr::Unit { span: (9, 10) },
                                    span: (9, 10),
                                }],
                                name: Name::Index(0),
                                span: (8, 10),
                            }),
                            Stmt::Expr {
                                expr: Expr::Unit { span: (11, 12) },
                                span: (11, 12),
                            },
                        ],
                        name: Name::Index(1),
                        span: (6, 12),
                    }),
                    Stmt::Func(Box::new(Func {
                        name: "a".to_string(),
                        param: Pattern::Unit { span: (18, 20) },
                        ret_ty: TypeExpr::Unit { span: (24, 26) },
                        body: Expr::Block(Box::new(Block {
                            stmts: vec![Stmt::Expr {
                                expr: Expr::Unit { span: (28, 29) },
                                span: (28, 29),
                            }],
                            name: Name::String("a".to_string()),
                            span: (27, 29),
                        })),
                        span: (13, 29),
                    })),
                    Stmt::Expr {
                        expr: Expr::Unit { span: (30, 31) },
                        span: (30, 31),
                    },
                ],
                name: Name::Index(0),
                span: (0, 31),
            })),
        );
    }
}
