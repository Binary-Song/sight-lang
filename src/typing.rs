use crate::ast::*;
use crate::{
    ast::{Lit, Term, Ty},
    compat_serialize::IntoTreeWithContext,
};
use log::debug;
use std::{
    arch::x86_64,
    collections::{HashMap, HashSet, VecDeque},
    fmt::{format, Debug},
};
const log_enabled: bool = false;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Unit,
    Int,
    Bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Primitive(PrimitiveType),
    Func { lhs: Vec<Type>, rhs: Box<Type> },
    Var { name: i32 },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Context {
    var_types: VecDeque<(String, Type)>,
    last_var_name: i32,
    constraints: Vec<(Type, Type)>,
}

impl Context {
    fn empty() -> Self {
        Context {
            var_types: VecDeque::new(),
            last_var_name: 0,
            constraints: vec![],
        }
    }
}

fn fresh_var(context: &mut Context) -> Type {
    context.last_var_name += 1;
    Type::Var {
        name: context.last_var_name,
    }
}

fn constrain(lhs: Type, rhs: Type, context: &mut Context) {
    context.constraints.push((lhs, rhs));
}

fn syntax_type_to_real_type(ty: Ty) -> Type {
    match ty {
        Ty::Bool { span } => Type::Primitive(PrimitiveType::Bool),
        Ty::Int { span } => Type::Primitive(PrimitiveType::Int),
        Ty::Arrow { l, r, span } => Type::Func {
            lhs: vec![syntax_type_to_real_type(*l)],
            rhs: Box::new(syntax_type_to_real_type(*r)),
        },
    }
}

fn type_of(term: Term, context: &mut Context) -> Type {
    match term {
        Term::Lit { value, span } => match value {
            Lit::Unit => Type::Primitive(PrimitiveType::Unit),
            Lit::Int(_) => Type::Primitive(PrimitiveType::Int),
            Lit::Bool(_) => Type::Primitive(PrimitiveType::Bool),
        },
        Term::Var { name, span } => context
            .var_types
            .iter()
            .find(|(var_name, ty)| *var_name == name)
            .unwrap()
            .1
            .clone(),
        Term::App { callee, args, span } => {
            let result_rhs_type = fresh_var(context);
            let callee_type = type_of(*callee, context);
            let arg_types = args
                .iter()
                .map(|arg| type_of(arg.clone(), context))
                .collect::<Vec<_>>();
            let res_type = Type::Func {
                lhs: arg_types,
                rhs: Box::new(result_rhs_type),
            };
            constrain(callee_type, res_type.clone(), context);
            res_type
        }
        Term::Func {
            params,
            ret_ty,
            body,
            span,
        } => {
            let mut param_count = 0;
            let mut param_types = vec![];
            for param in params {
                let param_ty = match param.ty {
                    // if annotated, use the type
                    Some(ty) => syntax_type_to_real_type(ty),
                    // else create a new type variable
                    None => fresh_var(context),
                };
                context
                    .var_types
                    .push_front((param.name.clone(), param_ty.clone()));
                param_count += 1;
                param_types.push(param_ty);
            }
            // type the body, with params in the context
            let body_ty = type_of(*body, context);
            // pop the context
            while param_count > 0 {
                context.var_types.pop_front();
                param_count -= 1;
            }
            // ret the func type
            Type::Func {
                lhs: param_types,
                rhs: Box::new(body_ty),
            }
        }
        Term::Let {
            name,
            ty,
            rhs,
            body,
            span,
        } => {
            // type the rhs
            let rhs_ty = type_of(*rhs, context);
            // if annotated, use the type
            let ty = match ty {
                Some(ty) => syntax_type_to_real_type(ty),
                None => fresh_var(context),
            };
            // constrain the rhs to the type
            constrain(rhs_ty, ty.clone(), context);
            // add the var to the context
            context.var_types.push_front((name.clone(), ty));
            // type the body
            let body_ty = type_of(*body, context);
            // pop the context
            context.var_types.pop_front();
            body_ty
        }
        Term::Seq { seq, .. } => seq
            .iter()
            .map(|term| type_of(term.clone(), context))
            .last()
            .unwrap_or(Type::Primitive(PrimitiveType::Unit)),
        Term::Op { .. } => Type::Func {
            lhs: vec![Type::Primitive(PrimitiveType::Int)],
            rhs: Box::new(Type::Primitive(PrimitiveType::Int)),
        },
    }
}

fn apply_solution(ty: Type, sln: &HashSet<(Type, Type)>) -> Type {
    match ty {
        Type::Primitive(p) => Type::Primitive(p),
        Type::Func { lhs, rhs } => {
            let new_lhs = lhs.into_iter().map(|t| apply_solution(t, sln)).collect();
            let new_rhs = apply_solution(*rhs, sln);
            Type::Func {
                lhs: new_lhs,
                rhs: Box::new(new_rhs),
            }
        }
        var @ Type::Var { name } => {
            if let Some((_, sln_dst)) = sln.iter().find(|(sln_src, _)| sln_src == &var) {
                sln_dst.clone()
            } else {
                var
            }
        }
    }
}

fn type_term(term: Term) -> Option<Type> {
    let mut context = Context::empty();
    let ty = type_of(term, &mut context);
    let constraints = unify(context.constraints.into_iter().collect())?;
    debug!("raw type = {ty}", ty = ty.dbg_print_root());
    let ty = apply_solution(ty, &constraints);
    debug!("final type: {ty}", ty = ty.dbg_print_root());
    return Some(ty);
}

fn fv(ty: &Type) -> Vec<Type> {
    match ty {
        Type::Primitive(_) => vec![],
        Type::Func { lhs, rhs } => {
            let mut result = vec![];
            for arg in lhs.iter() {
                result.extend(fv(arg));
            }
            result.extend(fv(rhs));
            result
        }
        Type::Var { name } => vec![ty.clone()],
    }
}

fn replace_constraints(
    constraints: HashSet<(Type, Type)>,
    from: &Type,
    to: &Type,
) -> HashSet<(Type, Type)> {
    let mut result: HashSet<(Type, Type)> = HashSet::new();
    for (lhs, rhs) in constraints {
        let new_lhs = if lhs == *from { to.clone() } else { lhs };
        let new_rhs = if rhs == *from { to.clone() } else { rhs };
        result.insert((new_lhs, new_rhs));
    }
    result
}

/// Given substitutions σ and γ, calculate the composite substitution σ ∘ γ
/// where σ is given by `sigma`, and γ is {`gamma_src` -> `gamma_dst`}
fn compose_substitutions(
    sigma: HashSet<(Type, Type)>,
    gamma_src: Type,
    gamma_dst: Type,
) -> HashSet<(Type, Type)> {
    let mut res: HashSet<(Type, Type)> = sigma
        .iter()
        .map(|(src, dst)| {
            if src != &gamma_src {
                (src.clone(), dst.clone())
            } else {
                match sigma.iter().find(|(a, _)| a == &gamma_dst) {
                    Some((_, b)) => (src.clone(), b.clone()),
                    None => (src.clone(), gamma_dst.clone()),
                }
            }
        })
        .collect();
    if res.iter().find(|(a, b)| a == &gamma_src).is_none() {
        res.insert((gamma_src.clone(), gamma_dst.clone()));
    }
    res
}

fn unify_with_rec(c: HashSet<(Type, Type)>, recursion: i32) -> Option<HashSet<(Type, Type)>> {
    fn unify_with_rec_impl(
        mut c: HashSet<(Type, Type)>,
        recursion: i32,
    ) -> Option<HashSet<(Type, Type)>> {
        let first = c.iter().next().cloned();
        if first.is_none() {
            return Some(HashSet::new());
        }
        let first = first.unwrap();
        c.remove(&first);
        let (lhs, rhs) = first;
        if lhs == rhs {
            return unify_with_rec(c, recursion + 1);
        }
        match (lhs, rhs) {
            (x @ Type::Var { .. }, t) | (t, x @ Type::Var { .. }) if !fv(&t).contains(&x) => {
                let c = replace_constraints(c, &x, &t);
                let c = unify_with_rec(c, recursion + 1)?;
                // ?? Subst != Constr !!!
                let c = compose_substitutions(c, x, t);
                Some(c)
            }
            (
                Type::Func {
                    lhs: s_lhs,
                    rhs: s_rhs,
                },
                Type::Func {
                    lhs: t_lhs,
                    rhs: t_rhs,
                },
            ) if s_lhs.len() == t_lhs.len() => {
                s_lhs.iter().zip(t_lhs.iter()).for_each(|(s, t)| {
                    c.insert((s.clone(), t.clone()));
                });
                c.insert((*s_rhs, *t_rhs));
                return unify_with_rec(c, recursion + 1);
            }
            _ => None,
        }
    }
    log::debug!(
        "[{recursion}]{spaces} unifying {tree}",
        tree = c.dbg_print_root(),
        spaces = " ".repeat(recursion as usize)
    );
    let result = unify_with_rec_impl(c, recursion);
    log::debug!(
        "[{recursion}]{spaces} unify result {result}",
        spaces = " ".repeat(recursion as usize),
        result = result.dbg_print_root()
    );
    result
}

fn unify(c: HashSet<(Type, Type)>) -> Option<HashSet<(Type, Type)>> {
    log::debug!("unify starts");
    let res = unify_with_rec(c, 1);
    log::debug!("unify final result: {res}", res = res.dbg_print_root());
    return res;
}

struct DebugPrintContext {
    parent_precedence: i32,
}

impl DebugPrintContext {
    fn new() -> Self {
        DebugPrintContext {
            parent_precedence: -1,
        }
    }
}

trait DebugPrint {
    fn dbg_print_raw(&self, ctx: &DebugPrintContext) -> String;
    /// None means never add parentheses
    fn precedence(&self) -> i32;
    fn dbg_print_root(&self) -> String {
        self.dbg_print_raw(&DebugPrintContext::new())
    }
    fn dbg_print(&self, ctx: &DebugPrintContext) -> String {
        let raw = self.dbg_print_raw(ctx);
        if ctx.parent_precedence == -1 || self.precedence() == -1 {
            raw
        } else if ctx.parent_precedence <= self.precedence() {
            format!("({raw})")
        } else {
            raw
        }
    }
}

impl DebugPrint for Type {
    fn precedence(&self) -> i32 {
        match self {
            Type::Primitive(_) => 0,
            Type::Var { .. } => 0,
            Type::Func { lhs, rhs } => 1,
        }
    }

    fn dbg_print_raw(&self, ctx: &DebugPrintContext) -> String {
        match self {
            Type::Primitive(p) => format!("{p:?}"),
            Type::Func { lhs, rhs } => {
                let lhs_str = lhs
                    .iter()
                    .map(|t| t.dbg_print(ctx))
                    .collect::<Vec<_>>()
                    .join(", ");
                if lhs.len() == 1 {
                    format!("{} -> {}", lhs_str, rhs.dbg_print(ctx))
                } else {
                    format!("({lhs_str}) -> {}", rhs.dbg_print(ctx))
                }
            }
            Type::Var { name } => format!("?{name}"),
        }
    }
}

impl DebugPrint for HashSet<(Type, Type)> {
    fn dbg_print_raw(&self, ctx: &DebugPrintContext) -> String {
        if self.is_empty() {
            return "∅".to_string();
        }
        self.iter()
            .map(|(lhs, rhs)| {
                format!(
                    "{lhs} = {rhs}",
                    lhs = lhs.dbg_print(ctx),
                    rhs = rhs.dbg_print(ctx)
                )
            })
            .collect::<Vec<_>>()
            .join(", ")
    }
    fn precedence(&self) -> i32 {
        -1
    }
}

impl<T> DebugPrint for Option<T>
where
    T: DebugPrint,
{
    fn dbg_print_raw(&self, ctx: &DebugPrintContext) -> String {
        match self {
            Some(t) => t.dbg_print(ctx),
            None => "None".to_string(),
        }
    }
    fn precedence(&self) -> i32 {
        match self {
            Some(t) => t.precedence(),
            None => 0,
        }
    }
}

#[cfg(test)]
fn test_unify(source: &str) {
    use crate::ast::Term;
    use crate::compat_serialize::TreeContext;
    use crate::parser::parse;
    use pretty_assertions::assert_eq;
    colog::default_builder()
        .filter_level(log::LevelFilter::Debug)
        .init();
    let term = parse(source).unwrap();
    debug!(
        "term: {term}",
        term = term
            .into_tree(&TreeContext {
                src: source.to_string(),
                version: 1
            })
            .to_xml(&TreeContext {
                src: source.to_string(),
                version: 1
            })
    );
    let ty: Type = type_term(term).unwrap();
}

#[test]
fn unify_test1() {
    test_unify(
        "
{
    let a = 1;
    let f = fn (x) { x };
    let b = f(a);
    b
}
    ",
    )
}
