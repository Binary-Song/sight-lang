use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::Once;
use tracing::field::{Field, Visit};
use tracing_subscriber::fmt::{self, format::Writer, FormatEvent, FormatFields};
use tracing_subscriber::registry::LookupSpan;
use tracing_subscriber::util::SubscriberInitExt;
struct OnlyCurrentSpan;
pub struct StringVisitor {}

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
mod expr {
    use std::rc::Rc;

    use crate::ast::*;
    use crate::parser::tests::init_testing;
    use crate::LiteralValue;
    use crate::{ast::Expr, parser::Parser};
    use pretty_assertions::assert_eq;
    use tracing_subscriber;

    fn test_parse_expr_result(input: &str, expr: Expr) {
        init_testing();
        let mut parser = Parser::new(input);
        let actual = match parser.expr() {
            Ok(actual) => actual,
            Err(err) => {
                panic!("Err: {:?}, Parser tried: {:?}", err, parser.trials);
            }
        };
        let actual_str = actual.literal_value();
        println!("Got: {actual_str}");
        assert_eq!(actual, expr);
    }

    #[test]
    fn test_parse_expr_var() {
        test_parse_expr_result(
            "x",
            Expr::Var {
                name: "x".to_string(),
                span: (0, 1),
            },
        );
    }

    #[test]
    fn test_parse_expr_trivial_func() {
        test_parse_expr_result(
            "{ fn foo() -> (int, int) { (1,2) } }",
            Expr::Var {
                name: "x".to_string(),
                span: (0, 1),
            },
        );
    }

    #[test]
    fn test_parse_expr_func() {
        test_parse_expr_result(
            "{ fn foo(a: int, b: int) -> int { a + b + 1 } }",
            Expr::Block(Box::new(Block {
                stmts: vec![
                    Stmt::Func(Rc::new(Func {
                        name: "foo".to_string(),
                        param: Pattern::Tuple {
                            elems: vec![
                                Pattern::Var {
                                    name: "a".to_string(),
                                    ty: TypeExpr::Int { span: (12, 15) },
                                    span: (9, 10),
                                },
                                Pattern::Var {
                                    name: "b".to_string(),
                                    ty: TypeExpr::Int { span: (20, 23) },
                                    span: (17, 18),
                                },
                            ],
                            span: (9, 18),
                        },
                        ret_ty: TypeExpr::Int { span: (28, 31) },
                        body: Expr::Block(Box::new(Block {
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
    fn test_parse_app() {
        test_parse_expr_result(
            "
            { fn foo(a: int) -> int { 1 } foo 1 }",
            Expr::Block(Box::new(Block {
                stmts: vec![
                    Stmt::Func(Rc::new(Func {
                        name: "foo".to_string(),
                        param: Pattern::Tuple {
                            elems: vec![
                                Pattern::Var {
                                    name: "a".to_string(),
                                    ty: TypeExpr::Int { span: (12, 15) },
                                    span: (9, 10),
                                },
                                Pattern::Var {
                                    name: "b".to_string(),
                                    ty: TypeExpr::Int { span: (20, 23) },
                                    span: (17, 18),
                                },
                            ],
                            span: (9, 18),
                        },
                        ret_ty: TypeExpr::Int { span: (28, 31) },
                        body: Expr::Block(Box::new(Block {
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
    fn test_mutual_recusive() {
        test_parse_expr_result(
            "
            {
             fn foo() -> int { bar() }
             fn bar() -> int { foo() }
             foo() + bar()
            }",
            Expr::Var {
                name: "x".to_string(),
                span: (0, 1),
            },
        );
    }

    #[test]
    fn test_let() {
        test_parse_expr_result(
            "
            {
              let a = 1;
              let b = a + 1;
              b
            }",
            Expr::Var {
                name: "x".to_string(),
                span: (0, 1),
            },
        );
    }

    #[test]
    fn test_pattern() {
        test_parse_expr_result(
            "
            {
              let (a,b) = (1,2);
              let c = a + b; 
            }",
            Expr::Var {
                name: "x".to_string(),
                span: (0, 1),
            },
        );
    }
}
