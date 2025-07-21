use crate::container::*;
use sight_macros::{Internable, LiteralValue};
use std::hash::Hash;

/// An enum over different kinds of type ids.
#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub enum Type {
    Primitive(PrimitiveType),
    Function(FunctionType),
    Tuple(TupleType),
    Unknown(UnknownType),
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
    pub lhs: Id<Type>,
    pub rhs: Id<Type>,
}

impl FunctionType {
    pub fn to_type(self) -> Type {
        Type::Function(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct TupleType {
    pub elems: Vec<Id<Type>>,
}

impl TupleType {
    pub fn to_type(self) -> Type {
        Type::Tuple(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct UnknownType {
    pub index: usize,
}

impl UnknownType {
    pub fn to_type(self) -> Type {
        Type::Unknown(self)
    }
}
