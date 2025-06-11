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
use crate::parser::context::Constraint;
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

impl<'a> Context {
    fn new_with_builtins() -> Rc<Self> {
        let ctx = Self::new();
        let ctx = ctx.add_binding(Binding(
            BinaryOp::Add.name().to_string(),
            Bindable::Func(Type::Arrow {
                lhs: Box::new(Type::Tuple {
                    elems: vec![Type::Int, Type::Int],
                }),
                rhs: Box::new(Type::Int),
            }),
        ));
        let ctx = ctx.add_binding(Binding(
            BinaryOp::Sub.name().to_string(),
            Bindable::Func(Type::Arrow {
                lhs: Box::new(Type::Tuple {
                    elems: vec![Type::Int, Type::Int],
                }),
                rhs: Box::new(Type::Int),
            }),
        ));
        let ctx = ctx.add_binding(Binding(
            BinaryOp::Mul.name().to_string(),
            Bindable::Func(Type::Arrow {
                lhs: Box::new(Type::Tuple {
                    elems: vec![Type::Int, Type::Int],
                }),
                rhs: Box::new(Type::Int),
            }),
        ));
        let ctx = ctx.add_binding(Binding(
            BinaryOp::Div.name().to_string(),
            Bindable::Func(Type::Arrow {
                lhs: Box::new(Type::Tuple {
                    elems: vec![Type::Int, Type::Int],
                }),
                rhs: Box::new(Type::Int),
            }),
        ));
        let ctx = ctx.add_binding(Binding(
            UnaryOp::Neg.name().to_string(),
            Bindable::Func(Type::Arrow {
                lhs: Box::new(Type::Int),
                rhs: Box::new(Type::Int),
            }),
        ));
        let ctx = ctx.add_binding(Binding(
            UnaryOp::Pos.name().to_string(),
            Bindable::Func(Type::Arrow {
                lhs: Box::new(Type::Int),
                rhs: Box::new(Type::Int),
            }),
        ));
        ctx
    }

    fn add_bindings_in_patttern(self: Rc<Self>, pat: &TPattern) -> Rc<Context> {
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

#[derive(Debug)]
pub enum TypingErr {
    UnboundVar { span: (usize, usize) },
}

type TypingResult<T> = Result<T, TypingErr>;

impl TypeExpr {
    pub fn to_type(&self, ctx: Rc<Context>) -> TypingResult<Type> {
        match self {
            TypeExpr::Unit { .. } => Ok(Type::Unit),
            TypeExpr::Bool { .. } => Ok(Type::Bool),
            TypeExpr::Int { .. } => Ok(Type::Int),
            TypeExpr::Arrow { lhs, rhs, .. } => Ok(Type::Arrow {
                lhs: Box::new(lhs.to_type(ctx.clone())?),
                rhs: Box::new(rhs.to_type(ctx.clone())?),
            }),
            TypeExpr::Tuple { elems, .. } => {
                let mut result_elems = vec![];
                for elem in elems {
                    result_elems.push(elem.to_type(ctx.clone())?);
                }
                Ok(Type::Tuple {
                    elems: result_elems,
                })
            }
        }
    }
}

impl Pattern {
    fn to_typed(&self, ctx: Rc<Context>) -> TypingResult<TPattern> {
        match self {
            Pattern::Unit { span } => Ok(TPattern::Unit { span: *span }),
            Pattern::Var {
                name,
                ty: type_anno,
                span,
            } => Ok(TPattern::Var {
                name: name.clone(),
                ty: if let Some(type_anno) = type_anno {
                    type_anno.to_type(ctx)?
                } else {
                    Type::TypeVar {
                        index: ctx.fresh_var(),
                    }
                },
                span: *span,
            }),
            Pattern::Tuple { elems, span } => {
                let mut result_elems = vec![];
                for elem in elems {
                    result_elems.push(elem.to_typed(ctx.clone())?);
                }
                Ok(TPattern::Tuple {
                    ty: Type::Tuple {
                        elems: result_elems
                            .iter()
                            .map(|e| e.ty())
                            .collect::<Vec<_>>(),
                    },
                    span: *span,
                    elems: result_elems,
                })
            }
        }
    }
}

impl Block {
    fn stmts_to_typed(
        ctx: Rc<Context>,
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
                        lhs: Box::new(func.param.to_typed(ctx.clone())?.ty()),
                        rhs: Box::new(func.ret_ty.to_type(ctx.clone())?),
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
                    let lhs = lhs.to_typed(ctx.clone())?;
                    let ctx_with_lhs = ctx.clone().add_bindings_in_patttern(&lhs);
                    // rhs cannot use the new bindings in lhs
                    let rhs = rhs.to_typed(ctx.clone())?;
                    // letbody can use the new bindings
                    let body = Self::stmts_to_typed(ctx_with_lhs, stmts, span)?;
                    let span = (let_span.0, body.span().1);
                    let lhs_ty = lhs.ty();
                    let rhs_ty = rhs.ty();
                    let cons = ctx.add_constraint(Constraint {
                        lhs: lhs_ty,
                        rhs: rhs_ty,
                    });
                    let let_expr = TExpr::Let {
                        lhs,
                        rhs: Box::new(rhs),
                        body: Box::new(body),
                        cons,
                        span,
                    };
                    res_seq.push(let_expr);
                    break;
                }
                Stmt::Expr { expr, span } => res_seq.push(expr.to_typed(ctx.clone())?),
                Stmt::Func(func) => {
                    let tfunc = TExpr::Func {
                        func: Rc::new(TFunc {
                            name: func.name.clone(),
                            param: func.param.to_typed(ctx.clone())?,
                            ret_ty: func.ret_ty.to_type(ctx.clone())?,
                            body: func.body.to_typed(ctx.clone())?,
                            span: func.span,
                            func_ty: Type::Arrow {
                                lhs: Box::new(func.param.to_typed(ctx.clone())?.ty()),
                                rhs: Box::new(func.ret_ty.to_type(ctx.clone())?),
                            },
                        }),
                    };
                    res_seq.push(tfunc);
                }
                Stmt::Block(block) => {
                    res_seq.push(block.to_typed(ctx.clone())?);
                }
                Stmt::Empty { span } => {}
            }
        }
        return Ok(TExpr::Seq {
            ty: res_seq.last().map_or(Type::Unit, |e| e.ty()),
            seq: res_seq,
            span: span,
        });
    }

    pub fn to_typed(&self, ctx: Rc<Context>) -> TypingResult<TExpr> {
        Self::stmts_to_typed(ctx.clone(), &self.stmts, self.span)
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

    pub fn to_typed(&self, ctx: Rc<Context>) -> TypingResult<TExpr> {
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
                let ty = Self::find_var_by_name(name.as_str(), &ctx, *span)?.get_type();
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
            } => Ok(Expr::App {
                func: Box::new(Expr::Var {
                    name: op.name(),
                    span: op_span.clone(),
                }),
                arg: arg.clone(),
                span: span.clone(),
            }
            .to_typed(ctx.clone())?),
            Expr::BinaryOp {
                op,
                lhs: arg1,
                rhs: arg2,
                span,
                op_span,
            } => Ok(Expr::App {
                func: Box::new(Expr::Var {
                    name: op.name(),
                    span: op_span.clone(),
                }),
                arg: Box::new(Expr::Tuple {
                    elems: vec![*arg1.clone(), *arg2.clone()],
                    span: (arg1.span().0, arg2.span().1),
                }),
                span: *span,
            }
            .to_typed(ctx.clone())?),
            Expr::Block(block) => block.to_typed(ctx.clone()),
            Expr::App { func, arg, span } => {
                let func_typed = func.to_typed(ctx.clone())?;
                let arg_typed = arg.to_typed(ctx.clone())?;
                let func_ty = func_typed.ty();
                let arg_ty = arg_typed.ty();
                let ret_ty = Box::new(Type::TypeVar {
                    index: ctx.fresh_var(),
                });
                let constr = Constraint {
                    lhs: func_ty.clone(),
                    rhs: Type::Arrow {
                        lhs: Box::new(arg_ty),
                        rhs: ret_ty.clone(),
                    },
                };
                let cons = ctx.add_constraint(constr);
                Ok(TExpr::Application {
                    callee: Box::new(func_typed),
                    arg: Box::new(arg_typed),
                    ty: *ret_ty,
                    span: *span,
                    cons,
                })
            }
            Expr::Tuple { elems, span } => {
                let mut typed_elems = vec![];
                for elem in elems {
                    typed_elems.push(elem.to_typed(ctx.clone())?);
                }
                Ok(TExpr::Tuple {
                    span: *span,
                    ty: Type::Tuple {
                        elems: typed_elems.iter().map(|e| e.ty()).collect(),
                    },
                    elems: typed_elems,
                })
            }
        }
    }
}

#[cfg(test)]
mod test {
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
    use crate::LiteralValue;
    use function_name::named;
    use sight_macros::LiteralValue;
    use std::collections::VecDeque;
    use std::rc::Rc;
    use std::vec;

    #[test]
    fn test_type_expr() {
        let mut parser = Parser::new("{ let a = 1; a + 1 }");
        let expr = parser.expr().unwrap();
        let ctx = Context::new_with_builtins();
        println!("ctx = {ctx:?}");
        let t = expr.to_typed(ctx.clone()).unwrap().literal_value();
        println!("tree = {t}");
        for constraint in ctx.constraints().borrow().iter() {
            println!("constraint: {:?} == {:?}", constraint.lhs, constraint.rhs);
        }
    }
}
