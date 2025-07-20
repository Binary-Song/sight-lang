use crate::container::*;
use sight_macros::{Internable, LiteralValue};
use std::hash::Hash;

pub type TypeId<'a> = Id<'a, Type<'a, A>, A>;

/// An enum over different kinds of type ids.
#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, )]
pub enum Type<'a, A: Arena<Self>> {
    Primitive(PrimitiveType<'a, A>),
    Function(FunctionType<'a, A>),
    Tuple(TupleType<'a, A>),
    Unknown(UnknownType<'a, A>),
}

impl<'a, A: Arena<Type<'a, A>>> Type<'a, A> {
    pub fn unit() -> Type<'a, A> {
        Type::Tuple(TupleType { elems: vec![] })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub enum PrimitiveType<'a, A: Arena<Self>> {
    Bool,
    Int,
}

impl<'a, A: Arena<PrimitiveType<'a, A>>> PrimitiveType<'a, A> {
    pub fn to_type(self) -> Type<'a, A> {
        Type::Primitive(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct FunctionType<'a, A: Arena<Self>> {
    pub lhs: TypeId<'a, A>,
    pub rhs: TypeId<'a, A>,
}

impl<'a, A: Arena<FunctionType<'a, A>>> FunctionType<'a, A> {
    pub fn to_type(self) -> Type<'a, A> {
        Type::Function(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct TupleType<'a, A: Arena<Self>> {
    pub elems: Vec<TypeId<'a, A>>,
}

impl<'a, A: Arena<TupleType<'a, A>>> TupleType<'a, A> {
    pub fn to_type(self) -> Type<'a, A> {
        Type::Tuple(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct UnknownType<'a, A: Arena<Self>> {
    pub index: usize,
}

impl<'a, A: Arena<UnknownType<'a, A>>> UnknownType<'a, A> {
    pub fn to_type(self) -> Type<'a, A> {
        Type::Unknown(self)
    }
}
