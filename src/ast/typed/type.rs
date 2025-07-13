use crate::utils::interning::Interned;
use sight_macros::{Internable, LiteralValue};
use std::hash::Hash;

pub type TypeId = Interned<Type>;

/// An enum over different kinds of type ids.
#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Internable)]
pub enum Type {
    Primitive(PrimitiveType),
    Function(FunctionType),
    Tuple(TupleType),
    Variable(VariableType),
}

impl Type {
    pub fn unit() -> Type {
        Type::Tuple(TupleType { elems: vec![] })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub enum PrimitiveType {
    Bool,
    Int,
}

impl PrimitiveType {
    pub fn to_type(self) -> Type {
        Type::Primitive(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct FunctionType {
    pub lhs: TypeId,
    pub rhs: TypeId,
}

impl FunctionType {
    pub fn to_type(self) -> Type {
        Type::Function(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct TupleType {
    pub elems: Vec<TypeId>,
}

impl TupleType {
    pub fn to_type(self) -> Type {
        Type::Tuple(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct VariableType {
    pub index: usize,
}

impl VariableType {
    pub fn to_type(self) -> Type {
        Type::Variable(self)
    }
}
