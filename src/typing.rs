use crate::ast::*;
use crate::{
    ast::{Lit, Term, TypeExpr},
};
use log::debug;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::{format, Debug},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Unit,
    Int,
    Bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Primitive(PrimitiveType),
    Func { lhs: Box<Type>, rhs: Box<Type> },
    Var { name: i32 },
    Tuple { elems: Vec<Type> },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Context<'source> {
    var_types: VecDeque<(String, Type)>,
    last_var_name: i32,
    constraints: Vec<(Type, Type)>,
    recursion: i32,
    source: Option<&'source str>,
}

impl<'source> Context<'source> {
    fn new_with_source(source: &'source str) -> Self {
        Context {
            var_types: VecDeque::new(),
            last_var_name: 0,
            constraints: vec![],
            recursion: 0,
            source: Some(source),
        }
    }
    fn new() -> Self {
        Context {
            var_types: VecDeque::new(),
            last_var_name: 0,
            constraints: vec![],
            recursion: 0,
            source: None,
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

fn syntax_type_to_real_type(ty: TypeExpr) -> Type {
    match ty {
        TypeExpr::Bool { span } => Type::Primitive(PrimitiveType::Bool),
        TypeExpr::Int { span } => Type::Primitive(PrimitiveType::Int),
        TypeExpr::Arrow {
            lhs: l,
            rhs: r,
            span,
        } => Type::Func {
            lhs: Box::new(syntax_type_to_real_type(*l)),
            rhs: Box::new(syntax_type_to_real_type(*r)),
        },
        TypeExpr::Tuple { elems, span } => Type::Tuple {
            elems: elems.into_iter().map(syntax_type_to_real_type).collect(),
        },
    }
}

fn type_of(term: Term, context: &mut Context) -> Type {
    if let Some(src) = context.source {
        debug!(
            "[{recur}]{spaces}typing: {term}",
            recur = context.recursion,
            spaces = " ".repeat(context.recursion as usize),
            term = &src[term.span().0..term.span().1]
        );
    }
    context.recursion += 1;
    let ty = match term {
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
        Term::App { callee, arg, span } => {
            let callee_type = type_of(*callee, context);
            let expected_callee_rhs_type = fresh_var(context);
            let expected_callee_type = Type::Func {
                lhs: Box::new(type_of(*arg, context)),
                rhs: Box::new(expected_callee_rhs_type.clone()),
            };
            // constraint: callee should be a function that
            // happen to accept the exact arg types.
            constrain(callee_type, expected_callee_type, context);
            expected_callee_rhs_type
        }
        Term::Func {
            param,
            ret_ty,
            body,
            span,
        } => {
            let param_ty = match param.ty {
                // if annotated, use the type
                Some(ty) => syntax_type_to_real_type(ty),
                // else create a new type variable
                None => fresh_var(context),
            };
            context
                .var_types
                .push_front((param.name.clone(), param_ty.clone()));
            // type the body, with params in the context
            let body_ty = type_of(*body, context);
            context.var_types.pop_front();
            // ret the func type
            Type::Func {
                lhs: Box::new(param_ty),
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
            // if annotated, constrain the rhs to the annotated type
            let rhs_ty = match ty {
                Some(anno_ty) => {
                    let anno_ty = syntax_type_to_real_type(anno_ty);
                    constrain(rhs_ty.clone(), anno_ty, context);
                    rhs_ty
                }
                None => rhs_ty,
            };
            // add the var to the context
            context.var_types.push_front((name.clone(), rhs_ty));
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
            lhs: Box::new(Type::Primitive(PrimitiveType::Int)),
            rhs: Box::new(Type::Primitive(PrimitiveType::Int)),
        },
        Term::Tuple { elems, span } => {
            let elem_types = elems
                .into_iter()
                .map(|e| type_of(e, context))
                .collect::<Vec<_>>();
            Type::Tuple { elems: elem_types }
        }
    };
    context.recursion -= 1;
    debug!(
        "[{recur}]{spaces}result: {ty:?}",
        recur = context.recursion,
        spaces = " ".repeat(context.recursion as usize),
    );
    ty
}

fn type_term_with_context(term: Term, mut context: Context) -> Option<Type> {
    let ty = type_of(term, &mut context);
    let subst = unify(context.constraints.into_iter().collect())?;
    debug!("raw type = {ty}", ty = ty.dbg_print_root());
    let ty = subst.apply(ty);
    debug!("final type: {ty}", ty = ty.dbg_print_root());
    return Some(ty);
}

pub fn type_term(term: Term) -> Option<Type> {
    let context = Context::new();
    type_term_with_context(term, context)
}

pub fn type_term_with_source<'source>(term: Term, source: &'source str) -> Option<Type> {
    let context = Context::new_with_source(source);
    type_term_with_context(term, context)
}

fn fv(ty: &Type) -> Vec<Type> {
    match ty {
        Type::Primitive(_) => vec![],
        Type::Func { lhs, rhs } => {
            let mut result = vec![];
            result.extend(fv(lhs));
            result.extend(fv(rhs));
            result
        }
        Type::Var { name } => vec![ty.clone()],
        Type::Tuple { elems } => {
            let mut result = vec![];
            for elem in elems.iter() {
                result.extend(fv(elem));
            }
            result
        }
    }
}

struct Substitution {
    replacements: Vec<(i32, Type)>,
}

impl DebugPrint for Substitution {
    fn dbg_print_raw(&self, ctx: &DebugPrintContext) -> String {
        if self.replacements.is_empty() {
            return "∅".to_string();
        }
        self.replacements
            .iter()
            .rev()
            .map(|(from, to)| format!("T{from} => {to}", from = from, to = to.dbg_print(ctx)))
            .collect::<Vec<_>>()
            .join("; ")
    }

    fn precedence(&self) -> i32 {
        -1
    }
}

impl Substitution {
    fn new() -> Self {
        Substitution {
            replacements: vec![],
        }
    }

    /// Push a new substitution rule `from` -> `to` into the stack.
    fn push(&mut self, from: i32, to: Type) {
        self.replacements.push((from, to));
    }

    /// Apply the substitution stack from top to bottom.
    fn apply(&self, ty: Type) -> Type {
        fn apply_one(ty: Type, from: i32, to: &Type) -> Type {
            match ty {
                Type::Primitive(_) => ty,
                Type::Func { lhs, rhs } => {
                    let new_lhs = Box::new(apply_one(*lhs, from, to));
                    let new_rhs = Box::new(apply_one(*rhs, from, to));
                    Type::Func {
                        lhs: new_lhs,
                        rhs: new_rhs,
                    }
                }
                ty @ Type::Var { name } => {
                    if from == name {
                        to.clone()
                    } else {
                        ty
                    }
                }
                Type::Tuple { elems } => {
                    let new_elems = elems.into_iter().map(|e| apply_one(e, from, to)).collect();
                    Type::Tuple { elems: new_elems }
                }
            }
        }
        let mut ty = ty;
        for (from, to) in self.replacements.iter().rev() {
            ty = apply_one(ty, *from, to);
        }
        ty
    }
}

fn unify_with_rec(c: HashSet<(Type, Type)>, recursion: i32) -> Option<Substitution> {
    let mut c = c;
    log::debug!(
        "[{recursion}]{spaces} unifying {tree}",
        tree = c.dbg_print_root(),
        spaces = " ".repeat(recursion as usize)
    );
    let result = {
        let first = c.iter().next().cloned();
        if first.is_none() {
            return Some(Substitution::new());
        }
        let first = first.unwrap();
        c.remove(&first);
        let (lhs, rhs) = first;
        if lhs == rhs {
            return unify_with_rec(c, recursion + 1);
        }
        match (lhs, rhs) {
            (x @ Type::Var { name }, t) | (t, x @ Type::Var { name }) if !fv(&t).contains(&x) => {
                debug!(
                    "[{recursion}]{spaces} replacing {x} with {t}",
                    x = x.dbg_print_root(),
                    t = t.dbg_print_root(),
                    spaces = " ".repeat(recursion as usize)
                );
                let mut x_to_t = Substitution::new();
                x_to_t.push(name, t.clone());
                let c = c
                    .iter()
                    .map(|(s, t)| (x_to_t.apply(s.clone()), x_to_t.apply(t.clone())))
                    .collect::<HashSet<_>>();
                let mut subs = unify_with_rec(c, recursion + 1)?;
                subs.push(name, t);
                Some(subs)
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
            ) => {
                c.insert((*s_lhs, *t_lhs));
                c.insert((*s_rhs, *t_rhs));
                return unify_with_rec(c, recursion + 1);
            }
            _ => None,
        }
    };
    log::debug!(
        "[{recursion}]{spaces} unify result: {result}",
        spaces = " ".repeat(recursion as usize),
        result = result.dbg_print_root()
    );
    result
}

fn unify(c: HashSet<(Type, Type)>) -> Option<Substitution> {
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
            Type::Tuple { elems } => 100, // "," binds weakly
        }
    }

    fn dbg_print_raw(&self, ctx: &DebugPrintContext) -> String {
        match self {
            Type::Primitive(p) => format!("{p:?}"),
            Type::Func { lhs, rhs } => {
                format!("{} -> {}", lhs.dbg_print(ctx), rhs.dbg_print(ctx))
            }
            Type::Var { name } => format!("T{name}"),
            Type::Tuple { elems } => {
                if elems.is_empty() {
                    "()".to_string()
                } else if elems.len() == 1 {
                    format!("tup({})", elems[0].dbg_print(ctx))
                } else {
                    elems
                        .iter()
                        .map(|t| t.dbg_print(ctx))
                        .collect::<Vec<_>>()
                        .join(", ")
                }
            }
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
            .join("; ")
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
fn test_type_term(term: Term, expected: Option<Type>) {
    let ty: Option<Type> = type_term(term);
    assert_eq!(ty, expected);
}

#[test]
fn unify_test1() {
    // term before desugaring:
    // {
    //     let a = 1;
    //     let f = fn (x) { x };
    //     let b = f(a);
    //     b
    // }
    let term = Term::Seq {
        seq: vec![Term::Let {
            name: "a".to_string(),
            ty: None,
            rhs: Box::new(Term::Lit {
                value: Lit::Int(1),
                span: (15, 16),
            }),
            body: Box::new(Term::Seq {
                seq: vec![Term::Let {
                    name: "f".to_string(),
                    ty: None,
                    rhs: Box::new(Term::Func {
                        param: Box::new(Binding {
                            name: "x".to_string(),
                            ty: None,
                            span: (34, 35),
                        }),
                        ret_ty: None,
                        body: Box::new(Term::Seq {
                            seq: vec![Term::Var {
                                name: "x".to_string(),
                                span: (39, 40),
                            }],
                            span: (39, 40),
                        }),
                        span: (30, 42),
                    }),
                    body: Box::new(Term::Seq {
                        seq: vec![Term::Let {
                            name: "b".to_string(),
                            ty: None,
                            rhs: Box::new(Term::App {
                                callee: Box::new(Term::Var {
                                    name: "f".to_string(),
                                    span: (56, 57),
                                }),
                                arg: Box::new(Term::Var {
                                    name: "a".to_string(),
                                    span: (58, 59),
                                }),
                                span: (56, 60),
                            }),
                            body: Box::new(Term::Seq {
                                seq: vec![Term::Var {
                                    name: "b".to_string(),
                                    span: (66, 67),
                                }],
                                span: (61, 67),
                            }),
                            span: (48, 61),
                        }],
                        span: (43, 67),
                    }),
                    span: (22, 43),
                }],
                span: (17, 67),
            }),
            span: (7, 17),
        }],
        span: (7, 67),
    };
    test_type_term(term, Some(Type::Primitive(PrimitiveType::Int)))
}

#[test]
fn unify_test2() {
    // term before desugaring:
    // {
    //     let x = 1;
    //     let f = fn (x: bool) { x };
    //     f(x)
    // }
    test_type_term(
        Term::Seq {
            seq: vec![Term::Let {
                name: "x".to_string(),
                ty: None,
                rhs: Box::new(Term::Lit {
                    value: Lit::Int(1),
                    span: (15, 16),
                }),
                body: Box::new(Term::Seq {
                    seq: vec![Term::Let {
                        name: "f".to_string(),
                        ty: None,
                        rhs: Box::new(Term::Func {
                            param: Box::new(Binding {
                                name: "x".to_string(),
                                ty: Some(TypeExpr::Bool { span: (37, 41) }),
                                span: (34, 41),
                            }),
                            ret_ty: None,
                            body: Box::new(Term::Seq {
                                seq: vec![Term::Var {
                                    name: "x".to_string(),
                                    span: (45, 46),
                                }],
                                span: (45, 46),
                            }),
                            span: (30, 48),
                        }),
                        body: Box::new(Term::Seq {
                            seq: vec![Term::App {
                                callee: Box::new(Term::Var {
                                    name: "f".to_string(),
                                    span: (54, 55),
                                }),
                                arg: Box::new(Term::Var {
                                    name: "x".to_string(),
                                    span: (56, 57),
                                }),
                                span: (54, 58),
                            }],
                            span: (49, 58),
                        }),
                        span: (22, 49),
                    }],
                    span: (17, 58),
                }),
                span: (7, 17),
            }],
            span: (7, 58),
        },
        None,
    )
}

#[test]
fn unify_test3() {
    // term before desugaring:
    // {
    //     let x = 1;
    //     let f = fn (x: bool) { x };
    //     f(x)
    // }
    test_type_term(
        Term::Seq {
            seq: vec![Term::Let {
                name: "x".to_string(),
                ty: None,
                rhs: Box::new(Term::Lit {
                    value: Lit::Int(1),
                    span: (15, 16),
                }),
                body: Box::new(Term::Seq {
                    seq: vec![Term::Let {
                        name: "f".to_string(),
                        ty: None,
                        rhs: Box::new(Term::Func {
                            param: Box::new(Binding {
                                name: "x".to_string(),
                                ty: None,
                                span: (34, 35),
                            }),
                            ret_ty: Some(TypeExpr::Bool { span: (40, 44) }),
                            body: Box::new(Term::Seq {
                                seq: vec![Term::Var {
                                    name: "x".to_string(),
                                    span: (47, 48),
                                }],
                                span: (47, 48),
                            }),
                            span: (30, 50),
                        }),
                        body: Box::new(Term::Seq {
                            seq: vec![Term::App {
                                callee: Box::new(Term::Var {
                                    name: "f".to_string(),
                                    span: (56, 57),
                                }),
                                arg: Box::new(Term::Var {
                                    name: "x".to_string(),
                                    span: (58, 59),
                                }),
                                span: (56, 60),
                            }],
                            span: (51, 60),
                        }),
                        span: (22, 51),
                    }],
                    span: (17, 60),
                }),
                span: (7, 17),
            }],
            span: (7, 60),
        },
        None,
    )
}
