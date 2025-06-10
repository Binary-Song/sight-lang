use crate::ast::typed::Expr as TExpr;
use crate::ast::typed::Func as TFunc;
use crate::ast::typed::Lit as TLit;
use crate::ast::typed::Pattern as TPattern;
use crate::ast::typed::Type;
use crate::ast::typed::Typed;
use crate::ast::Func;
use crate::ast::*;
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::parser::context::Bindable;
use crate::parser::context::Binding;
use crate::parser::context::Context;
use crate::parser::context::ContextIter;
use crate::parser::Parser;
use crate::span;
use crate::span::Span;
use function_name::named;
use sight_macros::LiteralValue;
use std::collections::VecDeque;
use std::rc::Rc;
use std::vec;

impl<'a> Context<'a> {
    fn add_bindings_in_patttern(&self, pat: &TPattern) -> Context {
        fn collect_bindings_in_pattern(pat: &TPattern, bindings: &mut Vec<Binding>) {
            match pat {
                TPattern::Unit { span } => {}
                TPattern::Var { name, ty, .. } => {
                    bindings.push(Binding(name.clone(), Bindable::Var(ty.clone())));
                }
                TPattern::Tuple { elems, .. } => {
                    for elem in elems {
                        collect_bindings_in_pattern(elem, bindings);
                    }
                }
            }
        }
        let mut bindings = Vec::new();
        collect_bindings_in_pattern(pat, &mut bindings);
        self.add_bindings(bindings)
    }
}

pub enum TypingErr {
    UnboundVar { span: (usize, usize) },
}

type TypingResult<T> = Result<T, TypingErr>;

impl TypeExpr {
    pub fn to_type(&self, ctx: &Context) -> TypingResult<Type> {
        match self {
            TypeExpr::Unit { .. } => Ok(Type::Unit),
            TypeExpr::Bool { .. } => Ok(Type::Bool),
            TypeExpr::Int { .. } => Ok(Type::Int),
            TypeExpr::Arrow { lhs, rhs, .. } => Ok(Type::Arrow {
                lhs: Box::new(lhs.to_type(ctx)?),
                rhs: Box::new(rhs.to_type(ctx)?),
            }),
            TypeExpr::Tuple { elems, .. } => {
                let mut result_elems = vec![];
                for elem in elems {
                    result_elems.push(elem.to_type(ctx)?);
                }
                Ok(Type::Tuple {
                    elems: result_elems,
                })
            }
            TypeExpr::Unknown { span } => Ok(Type::TypeVar {
                index: ctx.fresh_var(),
            }),
        }
    }
}

impl Pattern {
    fn to_typed(&self, ctx: &Context) -> TypingResult<TPattern> {
        match self {
            Pattern::Unit { span } => Ok(TPattern::Unit { span: *span }),
            Pattern::Var {
                name,
                ty: type_anno,
                span,
            } => Ok(TPattern::Var {
                name: name.clone(),
                ty: type_anno.to_type(ctx)?,
                span: *span,
            }),
            Pattern::Tuple { elems, span } => {
                let mut result_elems = vec![];
                for elem in elems {
                    result_elems.push(elem.to_typed(ctx)?);
                }
                Ok(TPattern::Tuple {
                    elems: result_elems,
                    span: *span,
                })
            }
        }
    }
}

impl Block {
    fn block_stmts_to_typed(
        ctx: &Context,
        mut stmts: &[Stmt],
        mut span: (usize, usize),
    ) -> TypingResult<TExpr> {
        // first pass: add all function bindings to the context
        // so the user can do mutual recursions in these functions
        let mut bindings = vec![];
        for stmt in stmts {
            match stmt {
                Stmt::Func(func) => bindings.push(Binding(
                    func.name.clone(),
                    Bindable::Func(Type::Arrow {
                        lhs: Box::new(func.param.to_typed(ctx)?.get_type()),
                        rhs: Box::new(func.ret_ty.to_type(ctx)?),
                    }),
                )),
                _ => (),
            }
        }
        let ctx = ctx.add_bindings(bindings);
        // second pass: convert all statements to typed expressions
        // if we see a let stmt, we will tuck the rest of the sequence into the body of the let
        let mut res_seq: Vec<TExpr> = Vec::new();
        while !stmts.is_empty() {
            let first_stmt = &stmts[0];
            stmts = &stmts[1..];
            span = (first_stmt.span().1, span.1);
            match first_stmt {
                Stmt::Let {
                    lhs,
                    rhs,
                    span: let_span,
                } => {
                    let pat = lhs.to_typed(&ctx)?;
                    let ctx_with_lhs = ctx.add_bindings_in_patttern(&pat);
                    // rhs cannot use the new bindings in lhs
                    let rhs = rhs.to_typed(&ctx)?;
                    // letbody can use the new bindings
                    let body = Self::block_stmts_to_typed(&ctx_with_lhs, stmts, span)?;
                    let span = (let_span.0, body.span().1);
                    let let_expr = TExpr::Let {
                        lhs: pat,
                        rhs: Box::new(rhs),
                        body: Box::new(body),
                        span,
                    };
                    res_seq.push(let_expr);
                    break;
                }
                Stmt::Expr { expr, span } => res_seq.push(expr.to_typed(&ctx)?),
                Stmt::Func(func) => {
                    let tfunc = TExpr::Func {
                        func: Rc::new(TFunc {
                            name: func.name.clone(),
                            param: func.param.to_typed(&ctx)?,
                            ret_ty: func.ret_ty.to_type(&ctx)?,
                            body: func.body.to_typed(&ctx)?,
                            span: func.span,
                        }),
                    };
                    res_seq.push(tfunc);
                }
                Stmt::Block(block) => {
                    res_seq.push(block.to_typed(&ctx)?);
                }
                Stmt::Empty { span } => {}
            }
        }
        return Ok(TExpr::Seq {
            seq: res_seq,
            span: span,
        });
    }

    pub fn to_typed(&self, ctx: &Context) -> TypingResult<TExpr> {
        Self::block_stmts_to_typed(ctx, &self.stmts, self.span)
    }
}

impl Expr {
    fn find_var_by_name<'a>(
        name: &str,
        ctx: &'a Context,
        span: (usize, usize),
    ) -> TypingResult<&'a Bindable> {
        match ctx.find_self_by_name(name) {
            Some(binding) => Ok(&binding.1),
            None => Err(TypingErr::UnboundVar { span }),
        }
    }

    pub fn to_typed(&self, ctx: &Context) -> TypingResult<TExpr> {
        match self {
            Expr::Unit { span } => Ok(TExpr::Lit {
                value: TLit::Unit,
                span: *span,
            }),
            Expr::Int { value, span } => Ok(TExpr::Lit {
                value: TLit::Int(*value),
                span: *span,
            }),
            Expr::Bool { value, span } => Ok(TExpr::Lit {
                value: TLit::Bool(*value),
                span: *span,
            }),
            Expr::Var { name, span } => {
                let ty = Self::find_var_by_name(name.as_str(), ctx, *span)?.get_type();
                Ok(TExpr::Var {
                    name: name.clone(),
                    span: *span,
                    ty: ty,
                })
            }
            Expr::UnaryOp {
                op,
                arg,
                span,
                op_span,
            } => Ok(TExpr::Application {
                callee: Box::new(TExpr::Var {
                    name: op.name(),
                    span: *span,
                    ty: Expr::find_var_by_name(op.name().as_str(), ctx, *span)?.get_type(),
                }),
                arg: Box::new(arg.to_typed(ctx)?),
                span: *span,
            }),
            Expr::BinaryOp {
                op,
                lhs: arg1,
                rhs: arg2,
                span,
                op_span,
            } => Ok(TExpr::Application {
                callee: Box::new(TExpr::Var {
                    name: op.name(),
                    span: *span,
                    ty: Expr::find_var_by_name(op.name().as_str(), ctx, *span)?.get_type(),
                }),
                arg: Box::new(TExpr::Tuple {
                    elems: vec![arg1.to_typed(ctx)?, arg2.to_typed(ctx)?],
                    span: *span,
                }),
                span: *span,
            }),
            Expr::Block(block) => block.to_typed(ctx),
            Expr::App { func, arg, span } => Ok(TExpr::Application {
                callee: Box::new(func.to_typed(ctx)?),
                arg: Box::new(arg.to_typed(ctx)?),
                span: *span,
            }),
            Expr::Tuple { elems, span } => {
                let mut typed_elems = vec![];
                for elem in elems {
                    typed_elems.push(elem.to_typed(ctx)?);
                }
                Ok(TExpr::Tuple {
                    elems: typed_elems,
                    span: *span,
                })
            }
        }
    }
}

#[cfg(test)]
fn format_xml(src: &str) -> Result<String, xml::reader::Error> {
    use xml::{reader::ParserConfig, writer::EmitterConfig};
    let mut dest = Vec::new();
    let reader = ParserConfig::new()
        .trim_whitespace(true)
        .ignore_comments(false)
        .create_reader(src.as_bytes());
    let mut writer = EmitterConfig::new()
        .perform_indent(true)
        .normalize_empty_elements(false)
        .autopad_comments(false)
        .create_writer(&mut dest);
    for event in reader {
        if let Some(event) = event?.as_writer_event() {
            writer.write(event).unwrap();
        }
    }
    Ok(String::from_utf8(dest).unwrap())
}
