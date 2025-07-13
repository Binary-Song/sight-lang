use crate::ast::{id::*, typed::*};
use sight_macros::{Internable, LiteralValue, StaticInternable};
use std::{collections::HashMap, hash::Hash};

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

impl Into<Type> for PrimitiveType {
    fn into(self) -> Type {
        Type::Primitive(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct FunctionType {
    pub lhs: TypeId,
    pub rhs: TypeId,
}

impl Into<Type> for FunctionType {
    fn into(self) -> Type {
        Type::Function(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct TupleType {
    pub elems: Vec<TypeId>,
}

impl Into<Type> for TupleType {
    fn into(self) -> Type {
        Type::Tuple(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct VariableType {
    pub index: usize,
}

impl Into<Type> for VariableType {
    fn into(self) -> Type {
        Type::Variable(self)
    }
}
