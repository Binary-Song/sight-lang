// use std::collections::{vec_deque, VecDeque};
// use std::hash::{Hash, Hasher};
// use std::iter::Map;
// use std::ptr;
// use std::{
//     cell::RefCell,
//     collections::{HashMap, HashSet},
//     ops::DerefMut,
//     rc::Rc,
//     vec,
// };

use crate::{
    ast::{Literal, Term, Ty},
    utils::TestPrintV1,
};
use std::{
    arch::x86_64,
    collections::{HashMap, HashSet, VecDeque},
    fmt::{format, Debug},
};

use crate::ast::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Unit,
    Int,
    Bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    PrimitiveType(PrimitiveType),
    Function { lhs: Vec<Type>, rhs: Box<Type> },
    Variable { name: i32 },
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
    Type::Variable {
        name: context.last_var_name,
    }
}

fn constrain(lhs: Type, rhs: Type, context: &mut Context) {
    context.constraints.push((lhs, rhs));
}

fn syntax_type_to_real_type(ty: Ty) -> Type {
    match ty {
        Ty::Bool { span } => Type::PrimitiveType(PrimitiveType::Bool),
        Ty::Int { span } => Type::PrimitiveType(PrimitiveType::Int),
        Ty::Arrow { l, r, span } => Type::Function {
            lhs: vec![syntax_type_to_real_type(*l)],
            rhs: Box::new(syntax_type_to_real_type(*r)),
        },
    }
}

fn type_of(term: Term, context: &mut Context) -> Type {
    match term {
        Term::Literal { value, span } => match value {
            Literal::Unit => Type::PrimitiveType(PrimitiveType::Unit),
            Literal::Int(_) => Type::PrimitiveType(PrimitiveType::Int),
            Literal::Bool(_) => Type::PrimitiveType(PrimitiveType::Bool),
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
            let res_type = Type::Function {
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
            Type::Function {
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
            .unwrap_or(Type::PrimitiveType(PrimitiveType::Unit)),
        Term::Op { .. } => Type::Function {
            lhs: vec![Type::PrimitiveType(PrimitiveType::Int)],
            rhs: Box::new(Type::PrimitiveType(PrimitiveType::Int)),
        },
    }
}

fn apply_solution(ty: Type, sln: &HashSet<(Type, Type)>) -> Type {
    match ty {
        Type::PrimitiveType(p) => Type::PrimitiveType(p),
        Type::Function { lhs, rhs } => {
            let new_lhs = lhs.into_iter().map(|t| apply_solution(t, sln)).collect();
            let new_rhs = apply_solution(*rhs, sln);
            Type::Function {
                lhs: new_lhs,
                rhs: Box::new(new_rhs),
            }
        }
        var @ Type::Variable { name } => {
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
    let ty = apply_solution(ty, &constraints);
    return Some(ty);
}

fn fv(ty: &Type) -> Vec<Type> {
    match ty {
        Type::PrimitiveType(_) => vec![],
        Type::Function { lhs, rhs } => {
            let mut result = vec![];
            for arg in lhs.iter() {
                result.extend(fv(arg));
            }
            result.extend(fv(rhs));
            result
        }
        Type::Variable { name } => vec![ty.clone()],
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

/// Given replacements σ and γ, calculate the composite replacement σ ∘ γ
/// where σ is given by `sigma`, and γ is given by {`gamma_src` -> `gamma_dst`}
fn compose_replacements(
    sigma: HashSet<(Type, Type)>,
    gamma_src: Type,
    gamma_dst: Type,
) -> HashSet<(Type, Type)> {
    print!(
        "compose_replacements: sigma = {}, gamma src = {}, dst = {}\n",
        sigma.print_v1(),
        gamma_src.print_v1(),
        gamma_dst.print_v1()
    );
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
    print!("compose_replacements result: {}\n", res.print_v1());
    res
}

fn unify(mut C: HashSet<(Type, Type)>) -> Option<HashSet<(Type, Type)>> {
    static mut lvl: i32 = 0;

    fn unify_impl(mut C: HashSet<(Type, Type)>) -> Option<HashSet<(Type, Type)>> {
        if let Some(con) = C.iter().next().cloned() {
            C.remove(&con);
            let (lhs, rhs) = con;
            if lhs == rhs {
                return unify(C);
            } else {
                match (lhs, rhs) {
                    (X @ Type::Variable { .. }, T) | (T, X @ Type::Variable { .. })
                        if !fv(&T).contains(&X) =>
                    {
                        let mut C = unify(replace_constraints(C, &X, &T))?;
                        Some(compose_replacements(C, X, T))
                    }
                    (
                        Type::Function {
                            lhs: S_lhs,
                            rhs: S_rhs,
                        },
                        Type::Function {
                            lhs: T_lhs,
                            rhs: T_rhs,
                        },
                    ) if S_lhs.len() == T_lhs.len() => {
                        S_lhs.iter().zip(T_lhs.iter()).for_each(|(s, t)| {
                            C.insert((s.clone(), t.clone()));
                        });
                        C.insert((*S_rhs, *T_rhs));
                        return unify(C);
                    }
                    _ => None,
                }
            }
        } else {
            return Some(HashSet::new());
        }
    }
    unsafe {
        lvl += 1;
    }
    print!("[{}] unifying: {}\n", unsafe { lvl }, C.print_v1());
    let res: Option<HashSet<(Type, Type)>> = unify_impl(C);
    print!("[{}] unify result: {}\n", unsafe { lvl }, res.print_v1());
    unsafe {
        lvl -= 1;
    }
    return res;
}

impl TestPrintV1 for Context {
    fn print_v1(self: &Self) -> String {
        let mut string = String::new();
        for (lhs, rhs) in self.constraints.iter() {
            string += &format!("    {} = {}\n", lhs.print_v1(), rhs.print_v1());
        }
        format!("Constr (\n{})", string)
    }
}

impl TestPrintV1 for HashSet<(Type, Type)> {
    fn print_v1(self: &Self) -> String {
        let mut string = String::new();
        for (lhs, rhs) in self.iter() {
            string += &format!("    {} = {}\n", lhs.print_v1(), rhs.print_v1());
        }
        format!("Constr (\n{})", string)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn test() {
        // fn (x: Int) { x }
        let expr = parse("{ let f = fn (x, y) { x }; f(1,2)} ").unwrap();
        let mut context = Context::empty();
        let ty = type_of(expr.clone(), &mut context);
        println!(
            "Expr: \n{:?}\n Type: \n{}\n Context: \n{}\n",
            expr,
            ty.print_v1(),
            context.print_v1()
        );
        println!("\n\nSolu: {}", type_term(expr).print_v1());
    }
}
