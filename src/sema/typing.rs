use crate::ast::typed::Expr as TExpr;
use crate::ast::typed::Func as TFunc;
use crate::ast::typed::Lit as TLit;
use crate::ast::typed::Pattern as TPattern;
use crate::ast::typed::QualifiedName;
use crate::ast::typed::Type;
use crate::ast::typed::Typed;
use crate::ast::visitor::Visitor;
use crate::ast::*;
use crate::guarded_push;
use crate::parser::context::Binding;
use crate::parser::context::Constraint;
use crate::parser::context::ConstraintKind;
use crate::parser::context::Context;
use crate::parser::context::Path;
use crate::span::Span;
use std::collections::VecDeque;
use std::fmt::Display;
use std::rc::Rc;
use std::vec;

impl<'a> Context {
    pub fn new_with_builtins() -> Rc<Self> {
        fn new_infix_op(op: BinaryOp) -> Binding {
            Binding {
                name: op.name().to_string(),
                path: Path::Func {
                    qual_name: QualifiedName::new_from_vec(vec![typed::Name::String(op.name())]),
                },
                span: None,
                ty: Type::Arrow {
                    lhs: Box::new(Type::Tuple {
                        elems: vec![Type::Int, Type::Int],
                    }),
                    rhs: Box::new(Type::Int),
                },
            }
        }
        let ctx = Self::new();
        let ctx = ctx.add_binding(new_infix_op(BinaryOp::Add));
        let ctx = ctx.add_binding(new_infix_op(BinaryOp::Sub));
        let ctx = ctx.add_binding(new_infix_op(BinaryOp::Mul));
        let ctx = ctx.add_binding(new_infix_op(BinaryOp::Div));
        ctx
    }

    fn add_bindings_in_patttern(self: Rc<Context>, pat: &TPattern) -> Rc<Context> {
        fn collect_bindings_in_pattern(
            ctx: Rc<Context>,
            pat: &TPattern,
            bindings: &mut Vec<Binding>,
        ) {
            match pat {
                TPattern::Var { name, ty, span } => {
                    bindings.push(Binding {
                        name: name.clone(),
                        path: Path::LocalVar {
                            index: ctx.alloc_local_var(),
                        },
                        span: Some(*span),
                        ty: (ty.clone()),
                    });
                }
                TPattern::Tuple { elems, .. } => {
                    for elem in elems {
                        collect_bindings_in_pattern(ctx.clone(), elem, bindings);
                    }
                }
            }
        }
        let mut bindings = Vec::new();
        collect_bindings_in_pattern(self.clone(), pat, &mut bindings);
        self.add_bindings(bindings)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypingErr {
    UnboundVar { span: (usize, usize) },
    DuplicateBinding { binding: Binding },
    CannotUnify { lhs: Type, rhs: Type },
}

pub type TypingResult<T> = Result<T, TypingErr>;

impl TypeExpr {
    pub fn to_type(&self, ctx: Rc<Context>) -> TypingResult<Type> {
        match self {
            TypeExpr::Unit { .. } => Ok(Type::unit()),
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
            Pattern::Unit { span } => Ok(TPattern::unit(*span)),
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
                        index: ctx.alloc_type_var(),
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
        stmts: VecDeque<Stmt>,
        mut span: (usize, usize),
    ) -> TypingResult<TExpr> {
        fn collect_func_bindings(
            ctx: Rc<Context>,
            stmts: VecDeque<Stmt>,
        ) -> TypingResult<(VecDeque<Stmt>, Vec<Binding>)> {
            let mut out_fn_stmts = VecDeque::new();
            let mut out_other_stmts = VecDeque::new();
            let mut bindings = vec![];
            for stmt in stmts {
                match stmt {
                    // if func, then we add the typing info to the bindings
                    // and insert the func to the front of the output_stmts
                    // so it will not be tucked into a let body
                    Stmt::Func(ref func) => {
                        bindings.push(Binding {
                            path: Path::Func {
                                qual_name: ctx.qualify(typed::Name::String(func.name.clone())),
                            },
                            ty: (Type::Arrow {
                                lhs: Box::new(func.param.to_typed(ctx.clone())?.ty()),
                                rhs: Box::new(func.ret_ty.to_type(ctx.clone())?),
                            }),
                            name: func.name.clone(),
                            span: Some(func.span),
                        });
                        out_fn_stmts.push_back(stmt);
                    }
                    s => {
                        out_other_stmts.push_back(s);
                    }
                }
            }
            out_fn_stmts.extend(out_other_stmts);
            Ok((out_fn_stmts, bindings))
        }
        // first pass: add all function bindings to the context
        // so the user can do mutual recursions in these functions

        let (mut stmts, bindings) = collect_func_bindings(ctx.clone(), stmts)?;
        // check if names are unique
        let mut names = std::collections::HashSet::new();
        for binding in &bindings {
            if !names.insert(binding.name.clone()) {
                return Err(TypingErr::DuplicateBinding {
                    binding: binding.clone(),
                });
            }
        }
        let ctx = ctx.add_bindings(bindings);
        // second pass: convert all statements to typed expressions
        // if we see a let stmt, we will tuck the rest of the sequence into the body of the let
        let mut res_seq: Vec<TExpr> = Vec::new();
        let mut block_count = 0;
        while !stmts.is_empty() {
            let first_stmt = stmts.remove(0).unwrap();
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
                    res_seq.push(TExpr::Func {
                        func: Box::new(TFunc {
                            name: ctx.qualify(func.name.clone().into()),
                            param: func.param.to_typed(ctx.clone())?,
                            ret_ty: func.ret_ty.to_type(ctx.clone())?,
                            body: {
                                guarded_push!(ctx.state_refmut().local_var_count, 0, {
                                    func.body.to_typed_with_unsolved_constraints(ctx.clone())
                                })?
                            },
                            span: func.span,
                            func_ty: Type::Arrow {
                                lhs: Box::new(func.param.to_typed(ctx.clone())?.ty()),
                                rhs: Box::new(func.ret_ty.to_type(ctx.clone())?),
                            },
                        }),
                    });
                }
                Stmt::Block(block) => {
                    let x = block.to_typed(ctx.clone())?;
                    res_seq.push(x)
                }
                Stmt::Empty { .. } => {}
            }
        }
        return Ok(TExpr::Seq {
            ty: res_seq.last().map_or(Type::unit(), |e| e.ty()),
            seq: res_seq,
            span: span,
        });
    }

    /// Converts the Block to a typed expression.
    pub fn to_typed(&self, ctx: Rc<Context>) -> TypingResult<TExpr> {
        // a block introduces a new nameless scope.
        guarded_push!(ctx.state_refmut().name_stack, self.name.clone(), {
            Self::stmts_to_typed(ctx.clone(), self.stmts.clone().into(), self.span)
        })
    }
}

impl Expr {
    fn lookup<'a>(name: &str, ctx: &'a Context, span: (usize, usize)) -> TypingResult<&'a Binding> {
        match ctx.lookup(name) {
            Some(binding) => Ok(binding),
            None => Err(TypingErr::UnboundVar { span }),
        }
    }

    /// Converts the expression to a typed expression with unsolved constraints.
    /// The constraints will be stored in the context, and can be solved later
    /// using `unify_constraints`.
    pub fn to_typed_with_unsolved_constraints(&self, ctx: Rc<Context>) -> TypingResult<TExpr> {
        match self {
            Expr::Unit { span } => Ok(TExpr::unit(span.clone())),
            Expr::Int { value, span } => Ok(TExpr::Lit {
                value: TLit::Int(*value),
                span: *span,
            }),
            Expr::Bool { value, span } => Ok(TExpr::Lit {
                value: TLit::Bool(*value),
                span: *span,
            }),
            Expr::Var { name, span } => {
                let bind = Self::lookup(name.as_str(), &ctx, *span)?;
                Ok(TExpr::Var {
                    name: name.clone().into(),
                    span: *span,
                    ty: bind.ty.clone(),
                    path: bind.path.clone(),
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
                    index: ctx.alloc_type_var(),
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
                Ok(TExpr::App {
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

    /// Converts the expression to a typed expression with solved constraints.
    pub fn to_typed(&self) -> TypingResult<TExpr> {
        let ctx = Context::new_with_builtins();
        let mut expr = self.to_typed_with_unsolved_constraints(ctx.clone())?;
        let state = ctx.state();
        let cons = &mut state.borrow_mut().constraints;
        let subs = unify_constraints(cons)?;
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
            (
                ref lhs @ Type::Tuple { elems: ref elems1 },
                ref rhs @ Type::Tuple { elems: ref elems2 },
            ) => {
                // Unify the elements of the tuples
                if elems1.len() != elems2.len() {
                    return Err(TypingErr::CannotUnify {
                        lhs: lhs.clone(),
                        rhs: rhs.clone(),
                    });
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
                return Err(TypingErr::CannotUnify { lhs, rhs });
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
