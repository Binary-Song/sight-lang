use crate::{ast::typed::*, span::Span};
use sight_macros::LiteralValue;

//
// PATTERN
//
#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct VariablePattern {
    pub binding_id: BindingId,
    pub ty: TypeId,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct TuplePattern {
    pub elems: Vec<PatternId>,
    pub ty: TypeId,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, LiteralValue)]
pub enum PatternId {
    Variable(Id<VariablePattern>),
    Tuple(Id<TuplePattern>),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Pattern {
    Variable(VariablePattern),
    Tuple(TuplePattern),
}

impl PatternId {
    pub fn de(self, arena: &impl GetArena) -> Pattern {
        match self {
            PatternId::Variable(id) => Pattern::Variable(id.de(arena)),
            PatternId::Tuple(id) => Pattern::Tuple(id.de(arena)),
        }
    }
}