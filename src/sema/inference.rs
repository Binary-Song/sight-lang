use crate::{
    ast::typed::TypeId,
    sema::typing::{TypeError, TypeRes},
};
use std::collections::{HashMap, HashSet, VecDeque};

pub trait TypeIdMapper {
    fn id_to_ctype(&self, id: TypeId) -> Type;
    fn ctype_to_id(&mut self, ty: Type) -> Result<TypeId, ()>;
}

/// An 'abstracted' type that only cares about the things that
/// the type inference cares.
/// Can convert to and from a `TypeId`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// A type variable. The inference algorithm will try to
    /// find a concrete type for this variable.
    TypeVar(usize),
    /// A type that does not have structure.
    /// i.e. primitives.
    Leaf { tag: usize },
    /// A type that has other types as children. e.g. function/tuples
    NonLeaf { tag: usize, children: Vec<TypeId> },
}

/// Type equations
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constraint {
    pub lhs: TypeId,
    pub rhs: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Solution {
    /// Type var to type id
    pub slns: HashMap<usize, TypeId>,
}

impl Solution {
    pub fn new() -> Self {
        Solution {
            slns: HashMap::new(),
        }
    }

    pub fn get(&self, index: usize) -> Option<TypeId> {
        self.slns.get(&index).cloned()
    }

    pub fn len(&self) -> usize {
        self.slns.len()
    }
}

/// Check if ty_var occurs in ty
pub fn occurs_check_ty(ty_var: usize, ty: &Type, mapper: &mut impl TypeIdMapper) -> bool {
    match ty {
        Type::TypeVar(var) => {
            return *var == ty_var;
        }
        Type::Leaf { tag: _ } => false,
        Type::NonLeaf { tag: _, children } => {
            for child in children {
                if occurs_check(ty_var, *child, mapper) {
                    return true;
                }
            }
            false
        }
    }
}

/// Check if ty_var occurs in ty_id
pub fn occurs_check(ty_var: usize, ty_id: TypeId, mapper: &mut impl TypeIdMapper) -> bool {
    let ty = mapper.id_to_ctype(ty_id);
    occurs_check_ty(ty_var, &ty, mapper)
}

pub fn unify(
    constraints: &mut VecDeque<Constraint>,
    mapper: &mut impl TypeIdMapper,
) -> TypeRes<Solution> {
    fn replace_type_var_with_its_sln(
        ty_id: TypeId,
        sln: &Solution,
        mapper: &mut impl TypeIdMapper,
    ) -> TypeId {
        let ty = mapper.id_to_ctype(ty_id);
        match &ty {
            Type::TypeVar(var) => {
                if let Some(ty_id) = sln.get(*var) {
                    ty_id
                } else {
                    mapper.ctype_to_id(ty).unwrap()
                }
            }
            _ => ty_id,
        }
    }
    let mut solution = Solution::new();
    while let Some(constraint) = constraints.pop_front() {
        let lhs = replace_type_var_with_its_sln(constraint.lhs, &solution, mapper);
        let rhs = replace_type_var_with_its_sln(constraint.rhs, &solution, mapper);
        let lhs = mapper.id_to_ctype(lhs);
        let rhs = mapper.id_to_ctype(rhs);
        match (lhs, rhs) {
            (Type::TypeVar(var), other) | (other, Type::TypeVar(var))
                if !occurs_check_ty(var, &other, mapper) =>
            {
                fn map_type(
                    mapper: &mut impl TypeIdMapper,
                    src: Type,
                    var: usize,
                    dst: Type,
                ) -> Type {
                    match src {
                        Type::TypeVar(v) => {
                            if var == v {
                                dst
                            } else {
                                Type::TypeVar(v)
                            }
                        }
                        Type::Leaf { tag } => Type::Leaf { tag },
                        Type::NonLeaf { tag, children } => {
                            let mapped_children = children
                                .into_iter()
                                .map(|child| {
                                    let t = mapper.id_to_ctype(child);
                                    let t = map_type(mapper, t, var, dst.clone());
                                    mapper.ctype_to_id(t).unwrap()
                                })
                                .collect::<Vec<_>>();
                            Type::NonLeaf {
                                tag,
                                children: mapped_children,
                            }
                        }
                    }
                }

                // Update existing solutions: replace var with other
                for (_, tid) in solution.slns.iter_mut() {
                    let new_t = map_type(mapper, mapper.id_to_ctype(*tid), var, other.clone());
                    *tid = mapper.ctype_to_id(new_t).unwrap();
                }

                solution
                    .slns
                    .insert(var, mapper.ctype_to_id(other).unwrap());
            }
            (Type::Leaf { tag: lhs_tag }, Type::Leaf { tag: rhs_tag }) if lhs_tag == rhs_tag => {}
            (
                Type::NonLeaf {
                    tag: lhs_tag,
                    children: lhs_children,
                },
                Type::NonLeaf {
                    tag: rhs_tag,
                    children: rhs_children,
                },
            ) if lhs_tag == rhs_tag && lhs_children.len() == rhs_children.len() => {
                for (lhs_child, rhs_child) in lhs_children.iter().zip(rhs_children.iter()) {
                    constraints.push_back(Constraint {
                        lhs: *lhs_child,
                        rhs: *rhs_child,
                    });
                }
            }
            (lhs, rhs) => {
                let lhs = mapper.ctype_to_id(lhs).unwrap();
                let rhs = mapper.ctype_to_id(rhs).unwrap();
                return Err(TypeError::CannotUnify { lhs, rhs });
            }
        }
    }
    Ok(solution)
}
