use crate::ast::{raw as r, raw, typed};
use crate::ast::span::Span;
use crate::ast::typed as t;
use crate::ast::typed::BlockExpr;
use crate::ast::typed::FuncBinding;
use crate::ast::typed::FuncFullData;
use crate::ast::typed::GetTy;
use crate::ast::typed::IdBinding;
use crate::ast::typed::ParamBinding;
use crate::ast::typed::TupleType;
use crate::ast::typed::VarBinding;
use crate::container::Container;
use crate::container::Id;
use crate::container::Item;
use std::collections::HashSet;
use t::Binding;

type LocalVars = Vec<Id<VarBinding>>;

#[derive(Debug)]
pub enum TypingError {
    VariableNotFound(Id<String>),
    NameIsNotAVariable(Id<String>),
    TypeCheckingFailed {
        expected: Id<t::Type>,
        got: Id<t::Type>,
    },
    DuplicateName(Id<String>),
    CallingANonFunction {
        got: Id<t::Type>,
    },
    CallingWithAnIncorrectNumberOfArgs(),
    IncorrectArgType {
        expected: Id<t::Type>,
        got: Id<t::Type>,
    },
    ProjectionOnNonTuple,
    ProjBadIndex,
}

pub type TypingResult<T> = Result<T, TypingError>;

impl GetTy for r::BasicType {
    fn get_ty(&self, container: &mut Container) -> Id<t::Type> {
        match self {
            r::BasicType::Unit => t::Type::unit().enc(container),
            r::BasicType::Bool => t::PrimitiveType::Bool.upcast().enc(container),
            r::BasicType::Int => t::PrimitiveType::Int.upcast().enc(container),
        }
    }
}

impl r::TypeExpr {
    fn get_ty<'c>(&self, container: &'c mut impl Container) -> TypingResult<Id<t::Type>> {
        match self {
            r::TypeExpr::Basic { t, .. } => Ok(t.get_ty(container)),
            r::TypeExpr::Var { .. } => {
                todo!()
            }
            r::TypeExpr::Arrow { lhs, rhs, .. } => Ok(t::FunctionType {
                lhs: {
                    let mut tys = vec![];
                    for ty in lhs {
                        tys.push(ty.get_ty(container)?);
                    }
                    tys
                },
                rhs: rhs.get_ty(container)?,
            }
            .to_type()
            .enc(container)),
            r::TypeExpr::Tuple { elems, .. } | r::TypeExpr::ClosedTuple { elems, .. } => {
                let mut tuple_elems = vec![];
                for e in elems {
                    let ty = e.get_ty(container)?;
                    tuple_elems.push(ty);
                }
                Ok(t::TupleType { elems: tuple_elems }.to_type().enc(container))
            }
        }
    }
}

pub fn lookup_name(
    name: Id<String>,
    container: &mut Container,
    context: IdBinding,
) -> Option<IdBinding> {
    let mut current = Some(context);
    while let Some(id) = current {
        let binding = id.dec(container);
        if let Some(binding_name) = binding.name() {
            if binding_name == name {
                return Some(id);
            }
        }
        current = binding.parent();
    }
    None
}

/// Checks the type of an expr.
///
/// **Return values**:
///
/// `Expr`: A typed version of the `expr`.
///
/// `LocalVars`: `local_vars` extended with local vars introduced by this expr.
pub fn check_type_of_expr(
    expr: r::Expr,
    ty: Id<t::Type>,
    container: &mut Container,
    context: IdBinding,
    local_vars: LocalVars,
) -> TypingResult<(t::Expr, LocalVars)> {
    let expected_ty = ty;
    let (typed_expr, local_vars) = infer_type_for_expr(expr, container, context, local_vars)?;
    let got_ty = typed_expr.get_ty(container);
    if got_ty == expected_ty {
        Ok((typed_expr, local_vars))
    } else {
        Err(TypingError::TypeCheckingFailed {
            expected: expected_ty,
            got: got_ty,
        })
    }
}

/// Adds the recursive bindings in the block
/// to the binding list (to allow recursion).
///
/// **Returns**:
///
/// `binding` extended with recursive bindings in the block
pub fn collect_block_bindings(
    block: &r::Block,
    container: &mut Container,
    mut context: IdBinding,
) -> TypingResult<IdBinding> {
    let mut used_names = HashSet::<Id<String>>::new();
    /// Gets the binding id if this Stmt
    /// is a FuncStmt.
    fn handle_stmt(
        container: &mut Container,
        parent_binding: IdBinding,
        stmt: &r::Stmt,
    ) -> TypingResult<Option<Binding>> {
        match stmt {
            r::Stmt::Func { func } => {
                let name = func.name;
                let mut param_tys = Vec::new();
                let mut param_names = Vec::new();
                for p in &func.params {
                    let t = p.ty_ann.get_ty(container)?;
                    param_tys.push(t);
                    param_names.push(p.name);
                }
                let ret_ty = func.ret_ty_ann.get_ty(container)?;
                let binding = parent_binding.derive(
                    FuncBinding {
                        parent: None,
                        name,
                        param_tys,
                        ret_ty,
                        full_data: None,
                        param_names
                    }
                    .into(),
                );
                Ok(Some(binding))
            }
            _ => Ok(None),
        }
    }

    for stmt in &block.stmts {
        match handle_stmt(container, context, &stmt)? {
            Some(new_func_binding) => {
                let name = new_func_binding.name().unwrap();
                if used_names.contains(&name) {
                    // duplicate name
                    return Err(TypingError::DuplicateName(name));
                }
                used_names.insert(name);
                let binding_id = new_func_binding.enc(container);
                context = binding_id;
            }
            None => {}
        }
    }
    Ok(context)
}

pub fn infer_type_for_block(
    block: r::Block,
    container: &mut Container,
    context: IdBinding,
    mut local_vars: LocalVars,
) -> TypingResult<(t::Block, LocalVars)> {
    // pre-add the bindings in the block
    // to the context to allow recursion
    let mut ctx = collect_block_bindings(&block, container, context)?;
    // iterate over the stmts, potentially adding
    // more let-bindings to ctx and local_vars in the process
    let mut typed_stmts = vec![];
    for stmt in block.stmts {
        let typed_stmt;
        (typed_stmt, ctx, local_vars) = infer_type_for_stmt(stmt, container, ctx, local_vars)?;
        let typed_stmt = typed_stmt.enc(container);
        typed_stmts.push(typed_stmt);
    }
    // type the block's return value
    let typed_value = match block.value {
        Some(value) => {
            let typed_value;
            (typed_value, local_vars) = infer_type_for_expr(value, container, ctx, local_vars)?;
            typed_value
        }
        None => t::Expr::unit(block.span.clone().map(|span| Span(span.1, span.1))),
    };
    let typed_value = typed_value.enc(container);
    Ok((
        t::Block {
            stmts: typed_stmts,
            value: typed_value,
            span: block.span,
        },
        local_vars,
    ))
}

/// Infers the type of a stmt.
///
/// **Return values**:
///
/// - `Stmt`: A typed version of the `stmt`.
///
/// - `IdBinding`: `ctx` extended with non-rec bindings available after this stmt.
///
/// - `LocalVars`: `local_vars` extended with local vars introduced by this stmt.
pub fn infer_type_for_stmt(
    stmt: r::Stmt,
    container: &mut Container,
    context: IdBinding,
    mut local_vars: LocalVars,
) -> TypingResult<(t::Stmt, IdBinding, LocalVars)> {
    match stmt {
        // LetStmt adds a binding to the context
        r::Stmt::Let {
            lhs,
            rhs,
            ty_ann,
            name_span,
        } => {
            let rhs = match ty_ann {
                Some(ty_ann) => {
                    let ty_ann = ty_ann.get_ty(container)?;
                    let expr;
                    (expr, local_vars) =
                        check_type_of_expr(rhs, ty_ann, container, context, local_vars)?;
                    expr
                }
                None => {
                    let expr;
                    (expr, local_vars) = infer_type_for_expr(rhs, container, context, local_vars)?;
                    expr
                }
            };
            let ty = rhs.get_ty(container);
            let var_index = local_vars.len();
            let var_binding = VarBinding {
                parent: Some(context),
                name: lhs,
                ty: ty,
                index: var_index,
            };
            let var_binding_id = var_binding.enc(container);
            // add into context
            let ctx = IdBinding::Var(var_binding_id);
            // add into local vars
            local_vars.push(var_binding_id);
            let rhs_id = rhs.enc(container);
            let stmt = t::LetStmt {
                binding: var_binding_id,
                name_span: name_span,
            }
            .into();
            Ok((stmt, ctx, local_vars))
        }
        // FuncStmt does not add a new binding to the context
        // because we already did that in `collect_block_bindings`
        r::Stmt::Func { func } => {
            // find the function itself in the context
            // we have already added the func name into the
            // context using collect_block_bindings.
            let func_binding_id = match lookup_name(func.name, container, context).unwrap() {
                IdBinding::Func(id) => id,
                _ => panic!("Should be function binding"),
            };
            let mut func_binding = func_binding_id.dec(container);
            // add params into context
            let original_ctx = context;
            let mut ctx = original_ctx;
            let mut params = vec![];
            for (param_idx, r::Param { name, ty_ann, span }) in func.params.iter().enumerate() {
                let param_binding = ParamBinding {
                    parent: Some(ctx),
                    name: *name,
                    ty: func_binding.param_tys[param_idx],
                    index: param_idx,
                };
                let param_binding_id = param_binding.enc(container);
                params.push(param_binding_id);
                ctx = IdBinding::Param(param_binding_id);
            }
            // type the body
            let body = func.body;
            let local_vars_of_fn = vec![];
            let (typed_body, local_vars_of_fn) =
                infer_type_for_block(body, container, ctx, local_vars_of_fn)?;
            // fill in the full data and reassign
            func_binding.full_data = Some(FuncFullData {
                local_vars: local_vars_of_fn,
                params: params,
                body_id: typed_body.enc(container),
            });
            // put the new func binding (with full data) back into the container
            // with the same id
            container.rebind_f(func_binding_id, func_binding);
            // build return values
            let typed_fn_stmt = t::FunctionStmt {
                binding: func_binding_id,
                name_span: func.name_span,
            }
            .into();
            Ok((typed_fn_stmt, original_ctx, local_vars))
        }
        r::Stmt::Expr { expr } => {
            let typed_expr;
            (typed_expr, local_vars) = infer_type_for_expr(expr, container, context, local_vars)?;
            let stmt = t::Stmt::ExprStmt(t::ExprStmt { expr: typed_expr });
            Ok((stmt, context, local_vars))
        }
        r::Stmt::Empty { span } => Ok((t::EmptyStmt { span }.into(), context, local_vars)),
        r::Stmt::If { cond, then_br, else_br } => {
            
        }
    }
}

/// Infers the type of an expr.
///
/// **Return values**:
///
/// `Expr`: A typed version of the `expr`.
///
/// `LocalVars`: `local_vars` extended with local vars introduced by this expr.
pub fn infer_type_for_expr(
    expr: r::Expr,
    container: &mut Container,
    context: IdBinding,
    mut local_vars: LocalVars,
) -> TypingResult<(t::Expr, LocalVars)> {
    match expr { 
        r::Expr::Lit { value, span } => Ok(match value {
            r::Lit::Unit => (
                t::TupleExpr {
                    elems: vec![],
                    span,
                }
                .upcast(),
                local_vars,
            ),
            r::Lit::Bool(value) => (
                t::LitExpr {
                    value: t::Literal::Bool(value),
                    span,
                }
                .upcast(),
                local_vars,
            ),
            r::Lit::Int(value) => (
                t::LitExpr {
                    value: t::Literal::Int(value),
                    span,
                }
                .upcast(),
                local_vars,
            ),
        }),
        r::Expr::Var { name, span } => match lookup_name(name, container, context) {
            Some(binding_id) => {
                let binding = binding_id.dec(container);
                let result = match binding {
                    Binding::Var(VarBinding {
                        parent,
                        name,
                        ty,
                        index,
                    }) => t::VarExpr {
                        binding: binding_id,
                        name,
                        ty,
                        span,
                    }
                    .upcast(),
                    Binding::Func(FuncBinding {
                        parent,
                        name,
                        param_tys,
                        ret_ty,
                        full_data,
                    }) => t::VarExpr {
                        binding: binding_id,
                        name,
                        ty: t::FunctionType {
                            lhs: param_tys,
                            rhs: ret_ty,
                        }
                        .to_type()
                        .enc(container),
                        span,
                    }
                    .upcast(),
                    _ => return Err(TypingError::NameIsNotAVariable(name)),
                };
                Ok((result, local_vars))
            }
            None => Err(TypingError::VariableNotFound(name)),
        },
        r::Expr::App { func, args, span } => {
            // infer type of func
            let typed_func;
            (typed_func, local_vars) = infer_type_for_expr(*func, container, context, local_vars)?;
            let func_ty_id = typed_func.get_ty(container);
            let func_ty = func_ty_id.dec(container);
            // get param and ret ty
            let (param_tys, ret_ty) = match func_ty {
                t::Type::Function(t::FunctionType { lhs, rhs }) => (lhs, rhs),
                // the type has to be a function type
                _ => return Err(TypingError::CallingANonFunction { got: func_ty_id }),
            };
            // check arg count
            if param_tys.len() != args.len() {
                return Err(TypingError::CallingWithAnIncorrectNumberOfArgs());
            }
            // check arg types
            let mut typed_args = vec![];
            for (param_ty, arg) in param_tys.iter().zip(args.iter()) {
                let typed_arg;
                (typed_arg, local_vars) =
                    check_type_of_expr(arg.clone(), *param_ty, container, context, local_vars)?;
                typed_args.push(typed_arg.enc(container));
            }
            // output return type
            Ok((
                t::AppExpr {
                    callee: typed_func.enc(container),
                    args: typed_args,
                    ty: ret_ty,
                    span,
                }
                .upcast(),
                local_vars,
            ))
        }
        r::Expr::Tuple { elems, span } | r::Expr::ClosedTuple { elems, span } => {
            let mut typed_elems = vec![];
            for elem in elems {
                let typed_elem;
                (typed_elem, local_vars) = infer_type_for_expr(elem, container, context, local_vars)?;
                typed_elems.push(typed_elem.enc(container));
            }
            Ok((
                t::TupleExpr {
                    elems: typed_elems,
                    span,
                }
                .upcast(),
                local_vars,
            ))
        }
        r::Expr::Proj { tuple, index, span } => {
            // infer type of the tuple
            let typed_tuple;
            (typed_tuple, local_vars) = infer_type_for_expr(*tuple, container, context, local_vars)?;
            let tuple_ty_id = typed_tuple.get_ty(container);
            let tuple_ty = tuple_ty_id.dec(container);
            // get the elem ty at index
            let elem_ty = match tuple_ty {
                t::Type::Tuple(TupleType { elems }) => match elems.get(index) {
                    Some(e) => e.clone(),
                    None => return Err(TypingError::ProjBadIndex),
                },
                _ => return Err(TypingError::ProjectionOnNonTuple),
            };
            // return the typed version of the proj expr
            let typed_proj_expr = t::ProjExpr {
                tuple: typed_tuple.enc(container),
                index,
                span,
                ty: elem_ty,
            }
            .upcast();
            Ok((typed_proj_expr, local_vars))
        }
        r::Expr::Block(block) => {
            let typed_block;
            (typed_block, local_vars) = infer_type_for_block(*block, container, context, local_vars)?;
            let typed_block_expr = BlockExpr {
                block: typed_block.enc(container),
            };
            Ok((typed_block_expr.upcast(), local_vars))
        }
    }
}
