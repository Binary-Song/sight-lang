use std::collections::{vec_deque, VecDeque};
use std::hash::{Hash, Hasher};
use std::iter::Map;
use std::ptr;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    ops::DerefMut,
    rc::Rc,
    vec,
};

use crate::ast::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct VariableState {
    lower_bounds: VecDeque<SimpleType>,
    upper_bounds: VecDeque<SimpleType>,
    unique_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PrimitiveType {
    Unit,
    Int,
    Bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum SimpleType {
    Variable {
        st: Rc<RefCell<VariableState>>,
    },
    Primitive(PrimitiveType),
    Function {
        lhs: Vec<SimpleType>,
        rhs: SimpleType,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Type {
    Top,
    Bot,
    Union { lhs: Box<Type>, rhs: Box<Type> },
    Inter { lhs: Box<Type>, rhs: Box<Type> },
    Func { lhs: Box<Type>, rhs: Box<Type> },
    Recursive { name: String, ty: Box<Type> },
    TypeVar { name: String },
    Primitive(PrimitiveType),
}

impl Hash for SimpleType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            SimpleType::Variable { st } => ptr::hash(&**st, state),
            SimpleType::Primitive(pt) => pt.hash(state),
            SimpleType::Function { lhs, rhs } => {
                lhs.hash(state);
                rhs.hash(state);
            }
        }
    }
}

type Ctx = HashMap<String, SimpleType>;

fn type_term(term: Expr, ctx: Ctx, fresh_name: &mut i32, 
    cache: &mut HashSet<(SimpleType, SimpleType)>) -> SimpleType {
    match term {
        Expr::UnitLit { span } => SimpleType::Primitive(PrimitiveType::Unit),
        Expr::IntLit { value, span } => SimpleType::Primitive(PrimitiveType::Int),
        Expr::BoolLit { value, span } => SimpleType::Primitive(PrimitiveType::Bool),
        Expr::Var { name, span } => ctx
            .get(&name)
            .unwrap_or_else(|| panic!("Variable {} at {:?} is unbound.", name, span))
            .clone(),
        Expr::UnaryOp { op, arg, span } => todo!(),
        Expr::BinaryOp {
            op,
            arg1,
            arg2,
            span,
        } => todo!(),
        Expr::App { func, args, span } => {
            // type the callee
            let callee_ty =  type_term(*func.clone(), ctx.clone(), fresh_name, cache);

            // type the args
            let arg_types : Vec<_> = args.iter().map(|arg| {
              type_term(*func.clone(), ctx.clone(), fresh_name, cache)  
            }).collect();

            // create a type variable X for return type
            let output_type_var =  fresh_var(fresh_name);

            // constraint: callee should be of type:
            //  arg_types -> X
            constrain(callee_ty, SimpleType::Function { lhs: arg_types, rhs: output_type_var })

            // result is X
            output_type_var
        }
        Expr::Func {
            params,
            ret_ty,
            body,
            span,
        } => {
            let mut ctx = ctx.clone();
            let mut fn_ty_lhs_opt = None;
            // 给定body_ty，返回柯里化以后的函数类型。
            let mut fn_ty_rhs_closure  = Rc::new(RefCell::new(|body_ty: SimpleType| -> SimpleType {body_ty}));
            // currying
            for param in params.iter() {
                let param_ty_var = fresh_var(fresh_name);
                ctx.insert(param.name.clone(), param_ty_var.clone()); 
                match fn_ty_lhs_opt.clone() {
                    None => fn_ty_lhs_opt = Some(param_ty_var),
                    Some(fn_ty_lhs) =>   {
                        fn_ty_rhs_closure = Rc::new(RefCell::new(|body_ty| SimpleType::Function { lhs: Box::new(fn_ty_lhs.clone()), rhs: Box::new(fn_ty_rhs_closure.clone().borrow_mut()(body_ty)) }));
                        fn_ty_lhs_opt = Some(param_ty_var);
                    }
                }
            }
            let body_ty = type_term(*body, ctx, fresh_name, cache);
            let fn_ty_lhs = match fn_ty_lhs_opt { Some(t) => t, None => SimpleType::Primitive(PrimitiveType::Unit) };
            let fn_ty_rhs = fn_ty_rhs_closure.borrow()(body_ty);
            SimpleType::Function {
                lhs: Box::new(fn_ty_lhs),
                rhs: Box::new(fn_ty_rhs),
            } 
        },
        Expr::Let {
            name,
            ty,
            init,
            body,
            span,
        } => todo!(),
        Expr::Seq { seq, span } => todo!(),
    }
}

fn constrain(
    lhs: &mut SimpleType,
    rhs: &mut SimpleType,
    cache: &mut HashSet<(SimpleType, SimpleType)>,
) {
    if cache.contains(&(lhs.clone(), rhs.clone())) {
        return;
    } else {
        cache.insert((lhs.clone(), rhs.clone()));
    }
    match (lhs.clone(), rhs.clone()) {
        (SimpleType::Primitive(lhs), SimpleType::Primitive(rhs)) if lhs == rhs => return,
        (
            SimpleType::Function {
                lhs: ref mut l0,
                rhs: ref mut r0,
            },
            SimpleType::Function {
                lhs: ref mut l1,
                rhs: ref mut r1,
            },
        ) => {
            constrain(l1.as_mut(), l0.as_mut(), cache);
            constrain(r0.as_mut(), r1.as_mut(), cache);
        }
        (SimpleType::Variable { st: lhs }, ref mut rhs) => {
            lhs.borrow_mut().upper_bounds.push_front(rhs.clone());
            for lhs_lower_bound in &mut lhs.borrow_mut().lower_bounds {
                constrain(lhs_lower_bound, rhs, cache);
            }
        }
        (ref mut lhs, SimpleType::Variable { st: rhs }) => {
            rhs.borrow_mut().lower_bounds.push_front(lhs.clone());
            for rhs_lower_bound in &mut rhs.borrow_mut().upper_bounds {
                constrain(lhs, rhs_lower_bound, cache);
            }
        }
        _ => panic!("cannot constrain {:?} <: {:?}", lhs, rhs),
    }
}

fn fresh_var(fresh_name: &mut i32) -> SimpleType {
    *fresh_name += 1;
    SimpleType::Variable {
        st: Rc::new(RefCell::new(VariableState {
            lower_bounds: VecDeque::new(),
            upper_bounds: VecDeque::new(),
            unique_name: fresh_name.to_string(),
        })),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct PolarVar(pub Rc<RefCell<VariableState>>, pub bool);

impl Hash for PolarVar {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::hash(&*self.0, state);
        self.1.hash(state);
    }
}

fn coalesce_type(ty: SimpleType) -> Type {
    let mut recursive: HashMap<PolarVar, String> = HashMap::new();
    fn go(
        ty: SimpleType,
        polar: bool,
        in_process: HashSet<PolarVar>,
        recursive: &mut HashMap<PolarVar, String>,
        fresh_name: &mut i32,
    ) -> Type {
        match ty {
            SimpleType::Primitive(primitive_type) => Type::Primitive(primitive_type.clone()),
            SimpleType::Function { lhs, rhs } => Type::Func {
                lhs: Box::new(go(*lhs, !polar, in_process.clone(), recursive, fresh_name)),
                rhs: Box::new(go(*rhs, polar, in_process, recursive, fresh_name)),
            },
            SimpleType::Variable { st } => {
                let st_pol = PolarVar(st, polar);
                if in_process.contains(&st_pol) {
                    let type_var_name = match recursive.get(&st_pol) {
                        Some(name) => name.clone(),
                        None => {
                            if let SimpleType::Variable { st } = fresh_var(fresh_name) {
                                recursive.insert(st_pol, st.borrow().unique_name.clone());
                                st.borrow().unique_name.clone()
                            } else {
                                panic!("fresh var not returning var??")
                            }
                        }
                    };
                    Type::TypeVar {
                        name: type_var_name,
                    }
                } else {
                    let bounds = if polar {
                        st_pol.0.borrow().lower_bounds.clone()
                    } else {
                        st_pol.0.borrow().upper_bounds.clone()
                    };
                    let st_pol_set: HashSet<PolarVar> = HashSet::from([st_pol.clone()]);
                    let union = |a: HashSet<PolarVar>, b: HashSet<PolarVar>| -> HashSet<PolarVar> {
                        a.union(&b).map(|x| x.clone()).collect::<HashSet<_>>()
                    };
                    let in_proc_plus_st_pol: HashSet<PolarVar> = union(st_pol_set, in_process);

                    let type_union = |a: Type, b: Type| -> Type {
                        Type::Union {
                            lhs: Box::new(a),
                            rhs: Box::new(b),
                        }
                    };
                    let type_inter = |a: Type, b: Type| -> Type {
                        Type::Inter {
                            lhs: Box::new(a),
                            rhs: Box::new(b),
                        }
                    };
                    let mrg = if polar { type_union } else { type_inter };
                    let bound_types = bounds
                        .iter()
                        .map(|bound| {
                            go(
                                bound.clone(),
                                polar,
                                in_proc_plus_st_pol.clone(),
                                recursive,
                                fresh_name,
                            )
                        })
                        .collect::<Vec<_>>();
                    let res = bound_types.iter().fold(
                        Type::TypeVar {
                            name: st_pol.0.borrow().unique_name.clone(),
                        },
                        |arg0, arg1| mrg(arg0, arg1.clone()),
                    );
                    match recursive.get(&st_pol) {
                        Some(t) => Type::Recursive {
                            name: t.clone(),
                            ty: Box::new(res),
                        },
                        None => res,
                    }
                }
            }
            _ => todo!(),
        }
    }
    go(ty, true, HashSet::new(), &mut recursive, &mut 0)
}


#[test]
fn test()
{
    use crate::{ast_utils::TestPrintV1, parser::parse};

    let r0 = parse(
        "fn (f) { fn (x) { f(f(x)) } } ",
    )
    .unwrap();

    println!("{:?}", r0.print_v1());

    let
}
