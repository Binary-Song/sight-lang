use crate::ast::typed::Expr as TExpr;
use crate::ast::typed::Func as TFunc;
use crate::ast::typed::Lit as TLit;
use crate::ast::typed::Pattern as TPattern;
use crate::ast::typed::Type;
use crate::ast::typed::Typed;
use crate::ast::visitor::Visitor;
use crate::ast::*;
use crate::parser::context::Bindable;
use crate::parser::context::Binding;
use crate::parser::context::Constraint;
use crate::parser::context::ConstraintKind;
use crate::parser::context::Context;
use crate::span::Span;
use std::collections::VecDeque;
use std::fmt::Display;
use std::fs::OpenOptions;
use std::io::Write;
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
        ctx
    }

    fn add_bindings_in_patttern(self: Rc<Self>, pat: &TPattern) -> Rc<Context> {
        fn collect_bindings_in_pattern(pat: &TPattern, bindings: &mut Vec<Binding>) {
            match pat {
                TPattern::Unit { .. } => {}
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
    ErrorMsg(String),
}

pub type TypingResult<T> = Result<T, TypingErr>;

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
                        elems: result_elems.iter().map(|e| e.ty()).collect::<Vec<_>>(),
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
                    let rhs = rhs.to_typed_with_unsolved_constraints(ctx.clone())?;
                    // letbody can use the new bindings
                    let body = Self::stmts_to_typed(ctx_with_lhs, stmts, span)?;
                    let span = (let_span.0, body.span().1);
                    let lhs_ty = lhs.ty();
                    let rhs_ty = rhs.ty();
                    let cons = ctx.add_constraint(Constraint {
                        lhs: lhs_ty,
                        rhs: rhs_ty,
                        kind: ConstraintKind::Let,
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
                Stmt::Expr { expr, .. } => {
                    res_seq.push(expr.to_typed_with_unsolved_constraints(ctx.clone())?)
                }
                Stmt::Func(func) => {
                    let tfunc = TExpr::Func {
                        func: Box::new(TFunc {
                            name: func.name.clone(),
                            param: func.param.to_typed(ctx.clone())?,
                            ret_ty: func.ret_ty.to_type(ctx.clone())?,
                            body: func.body.to_typed_with_unsolved_constraints(ctx.clone())?,
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
                Stmt::Empty { .. } => {}
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

    pub fn to_typed_with_unsolved_constraints(&self, ctx: Rc<Context>) -> TypingResult<TExpr> {
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
                if let Ok(mut file) = OpenOptions::new()
                    .append(true)
                    .create(true)
                    .open("E:/sight-lang/1.txt")
                {
                    let _ = writeln!(file, "Found var {name} with type {ty:?}");
                }
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
            .to_typed_with_unsolved_constraints(ctx.clone())?),
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
            .to_typed_with_unsolved_constraints(ctx.clone())?),
            Expr::Block(block) => block.to_typed(ctx.clone()),
            Expr::App { func, arg, span } => {
                let func_typed = func.to_typed_with_unsolved_constraints(ctx.clone())?;
                let arg_typed = arg.to_typed_with_unsolved_constraints(ctx.clone())?;
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
                    kind: ConstraintKind::App,
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
                    typed_elems.push(elem.to_typed_with_unsolved_constraints(ctx.clone())?);
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

    pub fn to_typed(&self) -> TypingResult<TExpr> {
        let ctx = Context::new_with_builtins();
        let mut expr = self.to_typed_with_unsolved_constraints(ctx.clone())?;
        let mut constraints: std::collections::VecDeque<_> =
            ctx.constraints().borrow().clone().into_iter().collect();
        let subs = unify_constraints(&mut constraints)?;
        subs.apply_to_ast(&mut expr);
        Ok(expr)
    }
}

#[derive(Debug)]
pub struct Substitution {
    pub from: u32,
    pub to: Type,
}

#[derive(Debug)]
pub struct Substitutions {
    pub subs: Vec<Substitution>,
}

impl Substitution {
    pub fn apply_to_type_var(&self, ty: &mut Type) {
        match ty {
            Type::TypeVar { index } if *index == self.from => {
                *ty = self.to.clone();
            }
            _ => (),
        }
    }

    pub fn apply_to_ast<T: AST>(&self, ast: &mut T) {
        let _ = ast.accept(self);
    }
}

impl Visitor<()> for Substitution {
    fn visit_ttype(&self, ty: &mut Type) -> Result<(), ()> {
        self.apply_to_type_var(ty);
        Ok(())
    }
}

impl Substitutions {
    pub fn new() -> Self {
        Self { subs: Vec::new() }
    }

    pub fn add(&mut self, subs: Substitution) {
        self.subs.push(subs);
    }

    pub fn apply_to_type(&self, ty: &mut Type) {
        for sub in &self.subs {
            sub.apply_to_ast(ty);
        }
    }

    pub fn apply_to_ast<T: AST>(&self, ast: &mut T) {
        let _ = ast.accept(self);
    }

    pub fn pop(&mut self) -> Option<Substitution> {
        self.subs.pop()
    }
}

impl Visitor<()> for Substitutions {
    fn visit_ttype(&self, ty: &mut typed::Type) -> Result<(), ()> {
        self.apply_to_type(ty);
        Ok(())
    }
}

pub fn unify_constraints(
    constraints: &mut VecDeque<Constraint>,
) -> Result<Substitutions, TypingErr> {
    fn occurs(var_index: u32, ty: &Type) -> bool {
        match ty {
            Type::TypeVar { index } if *index == var_index => true,
            Type::Arrow { lhs, rhs } => occurs(var_index, lhs) || occurs(var_index, rhs),
            Type::Tuple { elems } => elems.iter().any(|e| occurs(var_index, e)),
            _ => false,
        }
    }

    let mut subs = Substitutions::new();
    while let Some(constraint) = constraints.pop_front() {
        match (constraint.lhs, constraint.rhs) {
            (Type::TypeVar { index: var_index }, other)
            | (other, Type::TypeVar { index: var_index })
                if !occurs(var_index, &other) =>
            {
                // If the variable does not occur in the other type, we can substitute it
                let sub = Substitution {
                    from: var_index,
                    to: other.clone(),
                };
                // Apply the substitution to all remaining constraints
                for c in constraints.iter_mut() {
                    sub.apply_to_ast(&mut c.lhs);
                    sub.apply_to_ast(&mut c.rhs);
                }
                subs.add(sub);
            }
            (
                Type::Arrow {
                    lhs: lhs1,
                    rhs: rhs1,
                },
                Type::Arrow {
                    lhs: lhs2,
                    rhs: rhs2,
                },
            ) => {
                // Unify the left-hand sides and right-hand sides of the arrows
                constraints.push_back(Constraint {
                    lhs: *lhs1,
                    rhs: *lhs2,
                    kind: ConstraintKind::Unify,
                });
                constraints.push_back(Constraint {
                    lhs: *rhs1,
                    rhs: *rhs2,
                    kind: ConstraintKind::Unify,
                });
            }
            (Type::Tuple { elems: elems1 }, Type::Tuple { elems: elems2 }) => {
                // Unify the elements of the tuples
                if elems1.len() != elems2.len() {
                    return Err(TypingErr::ErrorMsg(format!(
                        "Cannot unify tuples of different lengths: {} and {}",
                        elems1.len(),
                        elems2.len()
                    )));
                }
                for (e1, e2) in elems1.iter().zip(elems2.iter()) {
                    constraints.push_back(Constraint {
                        lhs: e1.clone(),
                        rhs: e2.clone(),
                        kind: ConstraintKind::Unify,
                    });
                }
            }
            (lhs, rhs) if lhs == rhs => {
                // If the types are equal, we can ignore this constraint
                continue;
            }
            (lhs, rhs) => {
                // If we reach here, we have an unresolvable constraint
                return Err(TypingErr::ErrorMsg(format!(
                    "Cannot unify types: {} and {}",
                    lhs, rhs
                )));
            }
        }
    }
    Ok(subs)
}

impl crate::ast::Expr {
    // pub fn to_typed(&self) -> crate::ast::typed::Expr
    // {

    // }
}

#[cfg(test)]
mod testing {
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
        println!("typed expr after = {expr}", expr = expr.literal_value());
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
                        ty: Type::Unit,
                        span: (32, 36),
                    }),
                    span: (2, 36),
                    cons: ConstraintHandle::new(1),
                }],
                ty: Type::Unit,
                span: (15, 36),
            },
        );
    }
}

impl Display for Substitution {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "T{} -> {}", self.from, self.to)
    }
}

impl Display for Substitutions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.subs.is_empty() {
            write!(f, "[]")
        } else {
            write!(f, "[")?;
            for (i, sub) in self.subs.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", sub)?;
            }
            write!(f, "]")
        }
    }
}
