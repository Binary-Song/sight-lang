use crate::ast::typed::*;
use sight_macros::{Internable, LiteralValue, StaticInternable};
use std::{collections::HashMap, hash::Hash};

//
// PATTERN
//
#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct VariablePattern {
    pub binding_id: BindingId,
    pub ty: TypeId,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct TuplePattern {
    pub elems: Vec<PatternId>,
    pub ty: TypeId,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, LiteralValue)]
pub enum PatternId {
    Variable(Id<VariablePattern>),
    Tuple(Id<TuplePattern>),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum PatternSum {
    Variable(VariablePattern),
    Tuple(TuplePattern),
}

impl PatternId {
    pub fn deref(self, arena: &Arena) -> PatternSum {
        match self {
            PatternId::Variable(id) => PatternSum::Variable(arena.deref(id).clone()),
            PatternId::Tuple(id) => PatternSum::Tuple(arena.deref(id).clone()),
        }
    }
}