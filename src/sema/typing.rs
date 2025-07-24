use crate::ast::raw as r;
use crate::ast::span::Span;
use crate::ast::typed as t;
use crate::ast::typed::GetTy;
use crate::container::Container;
use crate::container::Id;
use crate::container::Item;
use t::Binding;
use t::BindingData;

pub enum TypingError {
    VariableNotFound(Id<String>),
    NameIsNotAVariable(Id<String>),
    TypeCheckingFailed {
        expected: Id<t::Type>,
        got: Id<t::Type>,
    },
}

pub type TypingResult<T> = Result<T, TypingError>;

impl GetTy for r::BasicType {
    fn get_ty(&self, c: &mut impl Container) -> Id<t::Type> {
        match self {
            r::BasicType::Unit => t::Type::unit().encode_f(c),
            r::BasicType::Bool => t::PrimitiveType::Bool.to_type().encode_f(c),
            r::BasicType::Int => t::PrimitiveType::Int.to_type().encode_f(c),
        }
    }
}

// match c.lookup_name(*name) {
//     Some(binding_id) => {
//         let binding = binding_id.decode_f(c.container);
//         match binding.data {
//             BindingData:: { ty, .. } => Ok(t::VariableExpr {

//             }),
//             _ => Err(TypingError::NameIsNotAVariable(name)),
//         }
//     },
//     None => Err(TypingError::VariableNotFound(name)),
// }
impl r::TypeExpr {
    fn get_ty<'c>(&self, c: &'c mut impl Container) -> TypingResult<Id<t::Type>> {
        match self {
            r::TypeExpr::Basic { t, .. } => Ok(t.get_ty(c)),
            r::TypeExpr::Var { .. } => {
                todo!()
            }
            r::TypeExpr::Arrow { lhs, rhs, .. } => Ok(t::FunctionType {
                lhs: lhs.get_ty(c)?,
                rhs: rhs.get_ty(c)?,
            }
            .to_type()
            .encode_f(c)),
            r::TypeExpr::Tuple { elems, .. } | r::TypeExpr::ClosedTuple { elems, .. } => {
                let mut tuple_elems = vec![];
                for e in elems {
                    let ty = e.get_ty(c)?;
                    tuple_elems.push(ty);
                }
                Ok(t::TupleType { elems: tuple_elems }.to_type().encode_f(c))
            }
        }
    }
}

pub fn lookup_name(
    binding: Id<Binding>,
    container: &mut impl Container,
    name: Id<String>,
) -> Option<Id<Binding>> {
    let mut current = Some(binding);
    while let Some(id) = current {
        let binding = container.decode_f(id);
        if let Some(binding_name) = binding.data.name() {
            if binding_name == name {
                return Some(id);
            }
        }
        current = binding.parent;
    }
    None
}

pub fn check_type_of_expr(
    container: &mut impl Container,
    binding: Id<Binding>,
    expr: r::Expr,
    ty: r::TypeExpr,
) -> TypingResult<t::Expr> {
    let expected_ty = ty.get_ty(container)?;
    let typed_expr = infer_type_for_expr(container, binding, expr)?;
    let got_ty = typed_expr.get_ty(container);
    if got_ty == expected_ty {
        Ok(typed_expr)
    } else {
        Err(TypingError::TypeCheckingFailed {
            expected: expected_ty,
            got: got_ty,
        })
    }
}

/// Adds the func bindings in the block
/// to the binding list (to allow recursion).
pub fn collect_block_bindings(
    container: &mut impl Container,
    mut binding: Id<Binding>,
    block: &r::Block,
) -> TypingResult<Id<Binding>> {
    /// Gets the binding id if this Stmt
    /// is a FuncStmt.
    fn handle_stmt(
        container: &mut impl Container,
        parent_binding: Id<Binding>,
        stmt: &r::Stmt,
    ) -> TypingResult<Option<Id<Binding>>> {
        match stmt {
            r::Stmt::Func { func } => {
                let name = func.name;
                let mut param_tys = Vec::new();
                for p in &func.params {
                    let t = p.ty_ann.get_ty(container)?;
                    param_tys.push(t);
                }
                let ret_ty = func.ret_ty_ann.get_ty(container)?;
                let binding_data = BindingData::FunctionDecl {
                    name: name,
                    param_tys: param_tys,
                    ret_ty: ret_ty,
                    body_id: None,
                };
                let binding = parent_binding.derive(binding_data);
                let binding_id = binding.encode_f(container);
                Ok(Some(binding_id))
            }
            _ => Ok(None),
        }
    }

    for stmt in &block.stmts {
        match handle_stmt(container, binding, &stmt)? {
            Some(new_func_binding) => {
                binding = new_func_binding;
            }
            None => {}
        }
    }

    Ok(binding)
}

pub fn infer_type_for_block(
    container: &mut impl Container,
    ctx: Id<Binding>,
    block: r::Block,
) -> TypingResult<t::Block> {
    // pre-add the bindings in the block
    // to the context to allow recursion
    let mut ctx = collect_block_bindings(container, ctx, &block)?;
    // iterate over the stmts, potentially adding
    // more let-bindings in the process
    let mut typed_stmts = vec![];
    for stmt in block.stmts {
        let (new_ctx, typed_stmt) = infer_type_for_stmt(container, ctx, stmt)?;
        ctx = new_ctx;
        let typed_stmt = typed_stmt.encode_f(container);
        typed_stmts.push(typed_stmt);
    }
    // type the block's return value
    let typed_value = match block.value {
        Some(value) => infer_type_for_expr(container, ctx, value)?,
        None => t::Expr::tuple(block.span.clone().map(|span| Span(span.1, span.1))),
    };
    let typed_value = typed_value.encode_f(container);
    Ok(t::Block {
        stmts: typed_stmts,
        value: typed_value,
        span: block.span,
    })
}

pub fn infer_type_for_stmt(
    container: &mut impl Container,
    ctx: Id<Binding>,
    stmt: r::Stmt,
) -> TypingResult<(Id<Binding>, t::Stmt)> {
    match stmt {
        // LetStmt adds a binding to the context
        r::Stmt::Let {
            lhs,
            rhs,
            ty_ann: Some(ty_ann),
        } => {
            let rhs = check_type_of_expr(container, ctx, rhs, ty_ann)?;
            let ty = rhs.get_ty(container);
            let ctx = ctx
                .derive(BindingData::Variable { name: lhs, ty: ty })
                .encode_f(container);
            let rhs_id = rhs.encode_f(container);
            Ok((
                ctx,
                t::LetStmt {
                    binding: ctx,
                    lhs: lhs,
                    rhs: rhs_id,
                }
                .into(),
            ))
        }
        r::Stmt::Let {
            lhs,
            rhs,
            ty_ann: None,
        } => {
            let rhs = infer_type_for_expr(container, ctx, rhs)?;
            let ctx = ctx
                .derive(BindingData::Variable { name: lhs, ty: rhs.get_ty(container) })
                .encode_f(container);
            let rhs_id = rhs.encode_f(container);
            Ok((
                ctx,
                t::LetStmt {
                    binding: ctx,
                    lhs: lhs,
                    rhs: rhs_id,
                }
                .into(),
            ))
        }
        // FuncStmt does not add a new binding to the context
        // because we already did in `collect_block_bindings` 
        r::Stmt::Func { func } => {
            // 1. add params to binding
            // 2. type func body and reassign the binding in context
            let func_binding = lookup_name(ctx, container, func.name).unwrap();
            let mut func_binding = func_binding.decode_f(container);
            let data = match func_binding.data {
                BindingData::FunctionDecl { name, param_tys, ret_ty, body_id },
                _ => panic!()
            };

        }
        _ => todo!(),
    }
}

pub fn infer_type_for_expr(
    container: &mut impl Container,
    binding: Id<Binding>,
    expr: r::Expr,
) -> TypingResult<t::Expr> {
    match expr {
        r::Expr::Lit { value, span } => Ok(match value {
            r::Lit::Unit => t::TupleExpr {
                elems: vec![],
                span,
            }
            .into(),
            r::Lit::Bool(value) => t::LiteralExpr {
                value: t::Literal::Bool(value),
                span,
            }
            .into(),
            r::Lit::Int(value) => t::LiteralExpr {
                value: t::Literal::Int(value),
                span,
            }
            .into(),
        }),
        r::Expr::Var { name, span } => match lookup_name(binding, container, name) {
            Some(binding_id) => {
                let binding = container.decode_f(binding_id);
                match binding.data {
                    BindingData::Variable { ty, .. } => Ok(t::VariableExpr {
                        binding: binding_id,
                        name,
                        ty,
                        span,
                    }
                    .into()),
                    BindingData::FunctionDecl {
                        name,
                        param_type,
                        return_type,
                        body_id,
                    } => {
                        todo!()
                    }
                    _ => Err(TypingError::NameIsNotAVariable(name)),
                }
            }
            None => Err(TypingError::VariableNotFound(name)),
        },
        r::Expr::App { func, args, span } => {}
        r::Expr::Tuple { elems, span } => todo!(),
        r::Expr::ClosedTuple { elems, span } => todo!(),
        r::Expr::Proj { tuple, index, span } => todo!(),
        r::Expr::Block(block) => todo!(),
        r::Expr::Lambda { params, body, span } => todo!(),
    }
}
